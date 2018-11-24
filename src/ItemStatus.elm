module ItemStatus exposing
    ( ItemStatus
    , description
    , draft
    , published
    , saved
    , title
    )


type ItemStatus
    = Saved
    | Published
    | Draft


saved : ItemStatus
saved =
    Saved


published : ItemStatus
published =
    Published


draft : ItemStatus
draft =
    Draft


title : ItemStatus -> String
title status =
    case status of
        Saved ->
            "Saved"

        Published ->
            "Published"

        Draft ->
            "Draft"


description : ItemStatus -> String
description status =
    case status of
        Saved ->
            "Ready to publish!"

        Published ->
            "The item is live."

        Draft ->
            "There is a live version, but not this one!"
