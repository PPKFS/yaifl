# Module Cleanup Workflow

This document captures the established workflow and best practices for incrementally improving module documentation and API design in the Yaifl project.

## Core Principles

1. **Incremental Improvement**: Make small, verifiable changes that can be reviewed individually
2. **Consistency**: Follow established patterns from previously cleaned modules
3. **Proactive Design**: Add complete API coverage before it's needed
4. **Clear Communication**: Always propose changes in text before applying

## Workflow Steps

### 1. Module Selection
- Choose a module that needs documentation improvement
- Create todo list with identified improvement areas
- Prioritize items (high/medium/low)

### 2. Clarify Module Purpose and Domain Concepts
- When uncertain about module purpose or domain concepts:
- Read module code and related documentation
- Analyze usage patterns in other modules
- Identify unclear or ambiguous concepts
- Request clarification from maintainer for domain-specific questions
- Document architectural intent and design decisions
- Verify understanding before proceeding with documentation

### 3. Comprehensive Module Review
- Read the entire module to understand structure and purpose
- Analyze current documentation state and completeness

### 3. Architectural Analysis
- Analyze module's architecture and design patterns
- Check for architectural strengths and weaknesses
- Identify code complexity and potential simplifications
- Review performance considerations
- Examine usage patterns in other modules
- Assess design pattern consistency
- Identify potential refactoring opportunities
- Evaluate integration with other systems
- Document findings in architectural analysis report

### 3.5. Present Architectural Analysis for Review
- Summarize architectural strengths and weaknesses
- Highlight any potential issues or concerns
- Identify architectural patterns and anti-patterns
- Request feedback on architectural decisions
- Get approval before proceeding with documentation

### 3.7. Identify Architectural Improvements
- Identify specific architectural issues
- Propose concrete improvements for each issue
- Evaluate improvement feasibility and impact
- Prioritize improvements by benefit/cost ratio
- Document proposed changes clearly
- Request feedback on improvement proposals

### 4. Comprehensive Documentation Audit
- Systematically check all documentation for completeness and quality:
- Verify module header completeness (copyright, license, maintainer, description)
- Check all exported types have haddock documentation
- Check all exported functions have haddock documentation
- Check all type classes have haddock documentation
- Identify insufficient documentation (too brief, unclear, or unhelpful)
- Identify unprofessional documentation (negative tone, poor phrasing)
- Identify confusing documentation (ambiguous, misleading, or incorrect)
- Check for inconsistent terminology or naming
- Verify documentation follows established patterns and conventions
- Identify missing cross-references to related modules/types

### 5. Module Header
- Add comprehensive module header with:
  - Copyright (current year range)
  - MIT License declaration
  - Maintainer information
  - Module purpose description
  - Component list
  - "See also" section with related modules

### 3. Export List Organization
- Group related exports with section comments:
  ```haskell
  module Module.Name
    ( -- * Category 1
      Type1(..)
    , Type2(..)
    
    -- * Category 2
    , function1
    , function2
    ) where
  ```

### 4. Type Documentation
- Add haddock comments for all types
- Explain purpose and relationships
- Use cross-references to related modules
- Provide examples where helpful

### 5. Function Documentation
- Document all exported functions
- Use consistent phrasing: "Check if...", "Get the...", "Update the..."
- Explain what functions do, not how they work
- Use active voice

### 6. API Completeness
- Add missing property query functions
- Follow naming patterns (e.g., `roomIs*`, `thingIs*`)
- Check for downstream usage opportunities
- Replace inconsistent functions with unified patterns

### 7. Cross-References
- Add "See also" sections in module headers
- Reference related modules in type documentation
- Link to base types and systems

## Established Patterns

### Documentation Style
- **Naming**: "a thing" not "this thing", "a room" not "this room"
- **Voice**: Active voice preferred ("Check if X does Y" not "X is checked for Y")
- **Spelling**: British English conventions
- **Structure**: Complete sentences in haddock comments

### API Design
- **Query Functions**: `roomIsVisited`, `thingIsLit` pattern
- **Property Access**: `roomConnections`, `thingContainedBy` pattern
- **Boolean Queries**: "Check if" prefix for predicate functions
- **Getters**: "Get the" prefix for accessor functions

### Module Structure
```haskell
{-|
Module      : Module.Name
Copyright   : (c) Avery YYYY-YYYY
License     : MIT
Maintainer  : maintainer@email.com

Brief description of module purpose.

This module defines:
- Key component 1: brief explanation
- Key component 2: brief explanation

See also:
- Related.Module for X
- Another.Module for Y
-}

module Module.Name
  ( -- * Category 1
    Type1(..)
  , Type2(..)
  
  -- * Category 2
  , function1
  , function2
  ) where
```

## Workflow Automation

1. **Always propose changes in text first** before file modifications
2. **Update todo lists immediately** after completing each item
3. **Proceed automatically** to next item after applying changes
4. **Check for usage** when adding new functions
5. **Maintain consistency** with existing cleaned modules

## Module Cleanup Tracking

### Completed Modules
- [x] Yaifl.Rulebook (2026-03-14) - Added module header, improved export organization, added missing haddock documentation
- [x] Yaifl.Action (2026-03-14) - Added module header, improved export organization, added missing haddock documentation, fixed getAllRules bug
- [x] Yaifl.Activity (2026-03-14) - Added module header, improved export organization, added missing haddock documentation
- [x] Yaifl.Entity (2026-03-14) - Already cleaned up with proper documentation
- [x] Yaifl.Object.Kind (2026-03-14) - Already cleaned up with proper documentation
- [x] Yaifl.Thing.Kind (2026-03-14) - Already cleaned up with proper documentation
- [x] Yaifl.Room.Kind (2026-03-14) - Already cleaned up with proper documentation
- [x] Yaifl.Enclosing.Kind (2026-03-14) - Already fully documented, no improvements needed
- [x] Yaifl.Metadata (2026-03-14) - Enhanced module header, added missing haddock documentation for 6 functions
- [x] Yaifl.WorldModel (2026-03-14) - Enhanced module header with professional description
- [x] Yaifl.Effects.ObjectQuery (2026-03-14) - Enhanced module header, added missing haddock documentation for 6 functions
- [x] Yaifl.Effects.RuleEffects (2026-03-14) - Added complete module header and collector type documentation

### Modules Needing Cleanup
- [ ] Yaifl.Effects.Print
- [ ] Yaifl.Text.Responses
- [ ] Yaifl.Text.SayableValue
- [ ] Yaifl.Rulebooks.Accessibility
- [ ] Yaifl.Rulebooks.ActionProcessing
- [ ] Yaifl.Activities.ChoosingNotableLocaleObjects

### Progress Statistics
- **Total Modules Cleaned**: 12/19 (63%)
- **Bugs Fixed**: 1
- **Documentation Added**: 50+ haddock comments
- **Export Lists Improved**: 4 modules
- **Modules with Complete Documentation**: 12

## Quality Checklist

- [ ] Copyright year range is current
- [ ] All exported types have documentation
- [ ] All exported functions have documentation
- [ ] Export list is logically organized
- [ ] Cross-references to related modules added
- [ ] API is complete (all properties have query functions)
- [ ] Documentation uses consistent terminology
- [ ] British spelling conventions followed
- [ ] No "this thing"/"this room" phrasing
- [ ] Active voice used throughout

## Tools and Commands

```bash
# Search for direct property access that could use new query functions
grep -r "view.*#propertyName" .

# Check for inconsistent terminology
grep -r "this thing\|this room" .

# Verify export list organization
head -30 module/File.hs
```

## Lessons Learned

1. **Proactive API design** saves refactoring later
2. **Complete property coverage** makes API more discoverable
3. **Consistent patterns** reduce cognitive load
4. **Clear documentation** helps both users and maintainers
5. **Incremental changes** are easier to review and verify

## Future Improvements

- Consider adding property query functions for all data types
- Standardize error handling documentation
- Add more cross-module examples
- Consider adding since-version tags for new functions