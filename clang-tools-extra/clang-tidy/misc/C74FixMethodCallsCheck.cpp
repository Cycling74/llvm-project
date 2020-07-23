//===--- C74FixMethodCallsCheck.cpp - clang-tidy --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "C74FixMethodCallsCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "llvm/ADT/SmallVector.h"
#include "clang/Lex/Lexer.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

// want to replace method calls with CALL_METHOD(m, <args>)
// method calls come in different flavors: 
//  - in below examples m is a variable of type method
//  - x is the first parameter passed to a method call 
//  - p is a pointer to a struct that contains a field "m" of type method
// 1.   m(x, ...)                      -> CALL_METHOD(m, x, ...)
// 2.   (*m)(x, ...)                   -> CALL_METHOD(m, x, ...)
// 3.   p->m(x, ...)                   -> CALL_METHOD(p->m, x, ...)
// 4.   (*p->m)(x, ...)                -> CALL_METHOD(p->m, x, ...)
// 6.   zgetfn(o,_sym_foo)(x, ...)     -> CALL_METHOD(zgetfn(o,_sym_foo), x, ...)
// 7.   (zgetfn(o,_sym_foo))(x, ...)   -> CALL_METHOD(zgetfn(o,_sym_foo), x, ...)
// 8.   (*zgetfn(o,_sym_foo))(x, ...)  -> CALL_METHOD(zgetfn(0,_sym_foo), x, ...)

// the goal: 
// - match the entire callExpr() to get the beginning of the method call
//   - we will bind this to "methodcall"
// - find the SourceLocation for the first argument of the method call 
//   (note that we always have at least one arg)
//   back up the SourceLocation by one character 
// - given above we have SourceLocation for start to just before the args of the method

// we then match the callee part of the callExpr and put CALL_METHOD( before that 

// we can match the method calls above for all that don't have the * (1, 3, 6, and 7) with this: 
// match callExpr(callee( expr( hasType(typedefDecl(hasName("method"))) ).bind("meth")  ) ).bind("methodcall")

// we can match the method calls that use the * operator (2, 4, 5, and 8) with this: 
// match callExpr(callee( expr( hasDescendant(unaryOperator( hasUnaryOperand( hasType( typedefDecl( hasName("method")) ) )  ))).bind("meth") ) ).bind("methodcall")

std::string getCalleeBindNameForTypedefName(const char* typedefName)
{
  std::string str = std::string(typedefName) + "_callee";
  return str;
}

std::string getCallExprBindNameForTypedefName(const char* typedefName)
{
  std::string str = std::string(typedefName) + "_callexpr";
  return str;
}

void C74FixMethodCallsCheck::addMatcher(const char* typedefName, MatchFinder *Finder)
{
  std::string bindCallee = getCalleeBindNameForTypedefName(typedefName);
  std::string bindCallExpr = getCallExprBindNameForTypedefName(typedefName);

  // add matcher for method calls without the * before the function pointer
  Finder->addMatcher(
    callExpr(callee( expr( hasType(typedefDecl(hasName(typedefName))) ).bind(bindCallee)  ) ).bind(bindCallExpr),
    this
  );

  // add matcher for method calls with the * before the function pointer
  Finder->addMatcher(
    callExpr(callee( expr( hasDescendant(unaryOperator( hasUnaryOperand( hasType( typedefDecl( hasName(typedefName)) ) )  ))).bind(bindCallee) ) ).bind(bindCallExpr),
    this
  );
}

void C74FixMethodCallsCheck::registerMatchers(MatchFinder *Finder) 
{
  // add matcher for method calls without the * before the function pointer
  addMatcher("method", Finder);
  addMatcher("t_intmethod", Finder);
}

void C74FixMethodCallsCheck::doCheck(const MatchFinder::MatchResult &Result, const char *typedefName)
{
  const ASTContext *ASTCtx = Result.Context;
  const LangOptions &Opts = ASTCtx->getLangOpts();
  const SourceManager &SM = ASTCtx->getSourceManager();
  
  auto callExprBinding = getCallExprBindNameForTypedefName(typedefName);
  auto calleeBinding = getCalleeBindNameForTypedefName(typedefName);
  std::string macroName = std::string(typedefName)=="method" ? "CALL_METHOD" : "CALL_INTMETHOD";
  std::string macroInsertion = macroName + "(";

  if (const auto MatchedExpr = Result.Nodes.getNodeAs<CallExpr>(callExprBinding))
  {
    // there must be at least one argument to any method call
    if (MatchedExpr->getNumArgs() == 0) {
      diag(MatchedExpr->getExprLoc(), "method call doesn't have any arguments!");
      return;
    }

    if (MatchedExpr->getNumArgs() > 10) {
      // we currently have up to CALL_METHOD_9
      // this has the object and nine additional args for a total of 10
      diag(MatchedExpr->getExprLoc(), "method call has too many arguments!");
      return;
    }

    // we need to create two FixItHint steps:
    // 1. insert CALL_METHOD( at the start of the "methodcall"
    // 2. replace from just after callee expr ("meth" binding) to just before first arg with comma

    // okay, find the left parenthesis before the first argument
    SourceLocation methodBeginLoc = MatchedExpr->getBeginLoc();
    SourceLocation arg0Loc = MatchedExpr->getArg(0)->getBeginLoc();
    
    // ignore if detect a macro is already at the source location
    if (methodBeginLoc.isMacroID())
    {
      diag(methodBeginLoc, "found method call via macro, ignoring");
      return;
    }

    if (const auto methExpr = Result.Nodes.getNodeAs<Expr>(calleeBinding))
    {
      SourceLocation methEndLoc = Lexer::getLocForEndOfToken(methExpr->getEndLoc(), 0,
                                   SM,
                                   Opts);
      SmallVector<FixItHint, 2> fixItHints;
      fixItHints.push_back(
        FixItHint::CreateInsertion(methodBeginLoc, macroInsertion)
      );
      fixItHints.push_back(
        FixItHint::CreateReplacement(CharSourceRange::getCharRange(methEndLoc, arg0Loc), ", ")
      );

      diag(MatchedExpr->getExprLoc(), "found direct method call, replace with macro")
        << fixItHints
        ;
    }
    else 
    {
      diag(MatchedExpr->getExprLoc(), "can't get node for meth");
    }
  }
}

void C74FixMethodCallsCheck::check(const MatchFinder::MatchResult &Result) 
{
  doCheck(Result, "method");
  doCheck(Result, "t_intmethod");
}

} // namespace misc
} // namespace tidy
} // namespace clang
