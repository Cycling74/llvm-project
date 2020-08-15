//===--- Rewrite_entry_pointCheck.cpp - clang-tidy ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Rewrite_entry_pointCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

void Rewrite_entry_pointCheck::registerMatchers(MatchFinder *Finder) {
  auto ext_main_declaration = functionDecl(matchesName("ext_main"), isExpansionInMainFile(), unless(isDefinition()));
  Finder->addMatcher(ext_main_declaration.bind("ext_main_declaration"), this);


  auto entry_point = functionDecl(matchesName("ext_main"), isExpansionInMainFile(), isDefinition());
  Finder->addMatcher(entry_point.bind("entry"), this);

  auto new_fn = functionDecl(matchesName(".*_new$"), returns(pointsTo(voidType())),
                             isExpansionInMainFile(), unless(isDefinition()));

  Finder->addMatcher(new_fn.bind("ctor"), this);
}

void Rewrite_entry_pointCheck::check(const MatchFinder::MatchResult &Result)
{
  if (const auto *decl = Result.Nodes.getNodeAs<FunctionDecl>("ext_main_declaration")) {
    diag(decl->getLocation(), "removing ext_main declaration")
        << FixItHint::CreateRemoval(SourceRange(decl->getSourceRange().getBegin(),
                                                decl->getSourceRange().getEnd().getLocWithOffset(1) // trailing ;
                                                )
                                    );
    return;
  }

  const auto *ctorDecl = Result.Nodes.getNodeAs<FunctionDecl>("ctor");
  if (ctorDecl) {
    if (ctorDecl->getName().size() == 0)
      return;

    auto namedDecl = ctorDecl->getName();
    auto name = namedDecl.take_front(namedDecl.size() - 4);
    entry_point_name = std::string(name.data(), name.data() + name.size());

    diag(ctorDecl->getLocation(), "class name %0", DiagnosticIDs::Warning)
        << entry_point_name;
    return;
  }

  const auto *entryDecl = Result.Nodes.getNodeAs<FunctionDecl>("entry");
  if (entryDecl)
  {
    SourceRange declRange = entryDecl->getNameInfo().getSourceRange();

    if (entry_point_name.empty()) {
      StringRef filename = Result.SourceManager->getFilename(declRange.getBegin());
      StringRef filenameWithoutExtension = filename.drop_back(2);
      auto last_slash = filename.rfind('/');
      StringRef filenameWithoutExtensionAndDirs = filenameWithoutExtension.substr(last_slash + 1);
      entry_point_name = filenameWithoutExtensionAndDirs.str();
    }

    std::string newFunctionName = entry_point_name + "_main";

    std::replace(newFunctionName.begin(), newFunctionName.end(), '.', '_');

    diag(entryDecl->getLocation(), "renaming entry point")
      << FixItHint::CreateReplacement(declRange, newFunctionName);

    std::string trampoline =
        "\n\n"
        "#ifndef UMBRELLA\n"
        "void ext_main(void *r)\n{\n"
        "\t" + newFunctionName + "(r);\n"
        "}\n"
        "#endif";

    diag(entryDecl->getLocation(), "adding entry point")
      << FixItHint::CreateInsertion(entryDecl->getEndLoc().getLocWithOffset(1), trampoline);
  }
}

} // namespace misc
} // namespace tidy
} // namespace clang
