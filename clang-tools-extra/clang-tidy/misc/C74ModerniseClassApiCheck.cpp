//===--- C74ModerniseClassApiCheck.cpp - clang-tidy -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "C74ModerniseClassApiCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Lex/Lexer.h"

/*
 * TODO: if we want to refactor jitter externals, we need to rewrite `max_jit_classex_`
*/

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

void C74ModerniseClassApiCheck::registerMatchers(MatchFinder *Finder) {
  auto messlist_class_declaration = varDecl(isExpansionInMainFile(), isDefinition(), 
    matchesName("_class"), hasType(asString("t_messlist *")));
  Finder->addMatcher(messlist_class_declaration.bind("messlist_class_declaration"), this);

  auto class_declaration = varDecl(isExpansionInMainFile(), isDefinition(), matchesName("_class"));
  Finder->addMatcher(class_declaration.bind("class_declaration"), this);

  auto main_function = functionDecl(isExpansionInMainFile(), matchesName("_main"));
  for(const char *symbol : {"setup", "addbang", "addfloat", "addint", "addmess", "addinx", "addftx"}) {
    auto fncall = callExpr(callee(functionDecl(hasName(symbol))),
                           hasAncestor(main_function)
                           );
    Finder->addMatcher(fncall.bind(symbol), this);
  }

  auto main_function_without_class_register =
      functionDecl(isExpansionInMainFile(), matchesName("_main"),
                   unless(hasDescendant(callExpr(callee(functionDecl(hasName("class_register")))))),
                   hasDescendant(callExpr(callee(functionDecl(hasName("setup")))))
                   );

  auto return_main = returnStmt(hasAncestor(main_function_without_class_register));
  Finder->addMatcher(return_main.bind("return_main"), this);

  auto new_object = callExpr(callee(functionDecl(hasName("newobject"))));
  Finder->addMatcher(new_object.bind("newobject"), this);

  auto main_function_without_class_register_without_return =
      functionDecl(isExpansionInMainFile(), matchesName("_main"),
                   unless(hasDescendant(callExpr(callee(functionDecl(hasName("class_register")))))),
                   unless(hasDescendant(returnStmt())),
                   hasDescendant(callExpr(callee(functionDecl(hasName("setup"))))),
                   isDefinition()
                   );

  Finder->addMatcher(main_function_without_class_register_without_return.bind("class_reg_without_return"), this);
}


void C74ModerniseClassApiCheck::check(const MatchFinder::MatchResult &Result) {

  // helper
  auto getArg1String = [&](const CallExpr *MatchedDecl) {
    auto arg1Exp = MatchedDecl->getArg(1);
    LangOptions LangOpts = getLangOpts();
    StringRef subExprText = Lexer::getSourceText(CharSourceRange::getTokenRange(arg1Exp->getSourceRange()),
                                                 *Result.SourceManager, LangOpts);
    return subExprText.str();
  };


  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<VarDecl>("messlist_class_declaration")) {

    SourceRange typeRange = MatchedDecl->getTypeSourceInfo()->getTypeLoc().getNextTypeLoc().getLocalSourceRange();

    diag(typeRange.getBegin(), "convert t_messlist* to t_class", DiagnosticIDs::Warning)
        << FixItHint::CreateReplacement(typeRange, "t_class");
  }

  // infer class name
  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<VarDecl>("class_declaration")) {
    auto namedDecl = MatchedDecl->getName();
    auto name = namedDecl.take_front(namedDecl.size() - 6);
    class_var  = std::string(namedDecl.data(), namedDecl.data() + namedDecl.size());
    class_name = std::string(name.data(), name.data() + name.size());

    if (class_name.substr(0, 4) == "max_") {
      // "max_jit_foo" -> "jit.foo"
      std::replace(class_name.begin(), class_name.end(), '_', '.');
      class_name.erase(0, 4);
    }

    diag(MatchedDecl->getLocation(), "class name %0", DiagnosticIDs::Warning)
        << class_name;
  }

  // rewrite setup
  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<CallExpr>("setup")) {

    SourceRange arg0 = MatchedDecl->getArg(0)->getSourceRange();

    diag(arg0.getBegin(), "Renaming First Argument")
      << FixItHint::CreateReplacement(arg0,  "\"" + class_name + "\"");

    SourceRange functionName = MatchedDecl->getCallee()->getSourceRange();
    diag(arg0.getBegin(), "Renaming setup to class_new")
      << FixItHint::CreateReplacement(functionName, "class_new");

    diag(MatchedDecl->getSourceRange().getBegin(), "Create assignment")
      << FixItHint::CreateInsertion(MatchedDecl->getSourceRange().getBegin(), class_var + " = ");

      setup_replaced = true;
  }

  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<CallExpr>("addbang")) {
    SourceRange functionName = MatchedDecl->getCallee()->getSourceRange();
    diag(functionName.getBegin(), "Renaming addbang to class_addmethod")
      << FixItHint::CreateReplacement(functionName, "class_addmethod");

    SourceRange arg0 = MatchedDecl->getArg(0)->getSourceRange();

    diag(arg0.getBegin(), "fix class_addmethod arguments")
      << FixItHint::CreateInsertion(arg0.getBegin(),  "" + class_var + ", ");

    diag(MatchedDecl->getRParenLoc(), "fix class_addmethod arguments")
        << FixItHint::CreateInsertion(MatchedDecl->getRParenLoc(),  ", \"bang\", 0");
  }

  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<CallExpr>("addint")) {
    SourceRange functionName = MatchedDecl->getCallee()->getSourceRange();
    diag(functionName.getBegin(), "Renaming addint to class_addmethod")
      << FixItHint::CreateReplacement(functionName, "class_addmethod");

    SourceRange arg0 = MatchedDecl->getArg(0)->getSourceRange();

    diag(arg0.getBegin(), "fix class_addmethod arguments")
      << FixItHint::CreateInsertion(arg0.getBegin(),  "" + class_var + ", ");

    diag(MatchedDecl->getRParenLoc(), "fix class_addmethod arguments")
        << FixItHint::CreateInsertion(MatchedDecl->getRParenLoc(),  ", \"int\", A_LONG, 0");
  }

  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<CallExpr>("addinx")) {
    SourceRange functionName = MatchedDecl->getCallee()->getSourceRange();
    diag(functionName.getBegin(), "Renaming addinx to class_addmethod")
      << FixItHint::CreateReplacement(functionName, "class_addmethod");

    SourceRange arg0 = MatchedDecl->getArg(0)->getSourceRange();

    diag(arg0.getBegin(), "fix class_addmethod arguments")
      << FixItHint::CreateInsertion(arg0.getBegin(),  "" + class_var + ", ");

    SourceRange arg1 = MatchedDecl->getArg(1)->getSourceRange();
    diag(arg1.getBegin(), "remove inlet argument")
      << FixItHint::CreateRemoval(arg1);

    std::string method = "\"in" + getArg1String(MatchedDecl) + "\"";

    diag(MatchedDecl->getRParenLoc(), "fix class_addmethod arguments")
      << FixItHint::CreateInsertion(MatchedDecl->getRParenLoc(),  ", " + method + ", A_LONG, 0");
  }

  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<CallExpr>("addfloat")) {
    SourceRange functionName = MatchedDecl->getCallee()->getSourceRange();
    diag(functionName.getBegin(), "Renaming addfloat to class_addmethod")
      << FixItHint::CreateReplacement(functionName, "class_addmethod");

    SourceRange arg0 = MatchedDecl->getArg(0)->getSourceRange();

    diag(arg0.getBegin(), "fix class_addmethod arguments")
      << FixItHint::CreateInsertion(arg0.getBegin(),  "" + class_var + ", ");

    diag(MatchedDecl->getRParenLoc(), "fix class_addmethod arguments")
      << FixItHint::CreateInsertion(MatchedDecl->getRParenLoc(),  ", \"float\", A_FLOAT, 0");
  }

  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<CallExpr>("addflx")) {
    SourceRange functionName = MatchedDecl->getCallee()->getSourceRange();
    diag(functionName.getBegin(), "Renaming addfloat to class_addmethod")
      << FixItHint::CreateReplacement(functionName, "class_addmethod");

    SourceRange arg0 = MatchedDecl->getArg(0)->getSourceRange();

    diag(arg0.getBegin(), "fix class_addmethod arguments")
      << FixItHint::CreateInsertion(arg0.getBegin(),  "" + class_var + ", ");

    SourceRange arg1 = MatchedDecl->getArg(1)->getSourceRange();
    diag(arg1.getBegin(), "remove inlet argument")
      << FixItHint::CreateRemoval(arg1);

    std::string method = "\"ft" + getArg1String(MatchedDecl) + "\"";

    diag(MatchedDecl->getRParenLoc(), "fix class_addmethod arguments")
      << FixItHint::CreateInsertion(MatchedDecl->getRParenLoc(),  ", " + method + ", A_FLOAT, 0");
  }

  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<CallExpr>("addmess")) {
    SourceRange functionName = MatchedDecl->getCallee()->getSourceRange();
    diag(functionName.getBegin(), "Renaming addmess to class_addmethod")
      << FixItHint::CreateReplacement(functionName, "class_addmethod");

    SourceRange arg0 = MatchedDecl->getArg(0)->getSourceRange();

    diag(arg0.getBegin(), "fix class_addmethod arguments")
      << FixItHint::CreateInsertion(arg0.getBegin(),  "" + class_var + ", ");
  }

  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<ReturnStmt>("return_main")) {
    SourceLocation returnLocation = MatchedDecl->getReturnLoc();
    diag(returnLocation, "adding class_register")
        << FixItHint::CreateInsertion(returnLocation, "class_register(CLASS_BOX, " + class_var + ");\n\t");
  }

  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<FunctionDecl>("class_reg_without_return")) {
    if (MatchedDecl->hasBody() && MatchedDecl->getName().endswith("_main"))
    {
      SourceLocation returnLocation = MatchedDecl->getEndLoc();
      diag(returnLocation, "adding class_register")
          << FixItHint::CreateInsertion(returnLocation, "\tclass_register(CLASS_BOX, " + class_var + ");\n");
    }
  }

  if (const auto *MatchedDecl = Result.Nodes.getNodeAs<CallExpr>("newobject")) {
    if (setup_replaced) {
      SourceRange functionName = MatchedDecl->getCallee()->getSourceRange();
      diag(functionName.getBegin(), "Renaming newobject to object_alloc")
          << FixItHint::CreateReplacement(functionName, "object_alloc");
    }
  }
}

} // namespace misc
} // namespace tidy
} // namespace clang
