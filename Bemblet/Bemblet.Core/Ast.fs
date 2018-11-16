﻿module Ast

type Constraint =
    {
        key: string
        value: string
    }

type Kind =
    {
        name: string
        constraints: Constraint list
    }

type Expr =
    {
        symbol: string
        kind: Kind
        description: string
    }

type Component =
    | Expr of Expr
    | Verbatim of string

type Document =
    {
        components: Component list
    }