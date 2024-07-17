# retraitementGCMS

## How to install

1. Git clone the project
2. Change to the cloned directory, and create a `.Renviron` file that contains this:

```
DB_PASSPHRASE=mypassphrase
ADMIN_ID=myadmin
ADMIN_PASSWORD=mypassword
ADMIN1_MAIL=admin1@mail.com
ADMIN2_MAIL=admin2@mail.com
ADMIN3_MAIL=admin3@mail.com
```

3. In the same directory, type:

```
make install
```

## Dependencies

* Docker
