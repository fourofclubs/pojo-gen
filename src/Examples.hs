module Examples where

import Gen

appId :: Field
appId = Field "id" (Left "BigInteger") "The application id"

appName :: Field
appName = Field "name" (Left "String") "The application name"

appLocalizedNames :: Field
appLocalizedNames = Field "localizedNames" (Left "TreeMap<Locale, String>") "The localized app names, indexed by locale"

appArchived :: Field
appArchived = Field "archived" (Left "boolean") "Whether the application has been archived"

application :: Class
application = Class "Application" [appId, appName, appLocalizedNames, appArchived] "Application data"

userId :: Field
userId = Field "id" (Left "BigInteger") "The id of the user"

username :: Field
username = Field "username" (Left "String") "The user's username"

userApp :: Field
userApp = Field "application" (Right application) "The application the user is associated to"

user :: Class
user = Class "User" [userId, username, userApp] "User info"