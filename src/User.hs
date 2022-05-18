module User where

import Gen

userClss :: Class
userClss = Class "User_" [userId, userUsername, userName, userEmail, userPreferredLocale, userJurisdiction, userRhaId, userState, userTimezone, userAppIds, userAppPermissions] "User information"

nameClss :: Class
nameClss = Class "Name" [nameFirstFld, nameMiddleFld, nameLastFld, nameTitleFld] "The user's name fields"

nameFirstFld = fld "first" "String" "The user's first name"
nameMiddleFld = fld "middle" "Option<String>" "The user's middle name, if available"
nameLastFld = fld "last" "String" "The user's last name"
nameTitleFld = fld "title" "TreeMap<Locale, String>" "The user's localized title, indexed by locale"

userId = fld "id" "BigInteger" "The user id"
userUsername = fld "username" "String" "The user's username"
userName = fld' "name" nameClss "The user's name"
userEmail = fld "email" "Option<String>" "The user's email address, if available"
userPreferredLocale = fld "preferredLocale" "Locale" "The user's preferred locale, which defines the language this user should receive notifications in"
userJurisdiction = fld "jurisdiction" "Option<Jurisdiction>" "The user's jurisdiction, if available"
userRhaId = fld "rhaId" "Option<BigInteger>" "The id of the user's health region, if one has been assigned"
userState = fld "state" "State" "The current user state"
userTimezone = fld "timezone" "Option<TimeZone>" "The user's timezone, if known"
userAppIds = fld "applicationIds" "Set<BigInteger>" "The ids of the applications the user has access to"
userAppPermissions = fld "applicationPermissions" "HashMap<String, Set<String>>" "All application permissions the user has, indexed by application name"