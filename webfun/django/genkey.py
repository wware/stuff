from random import choice

print ("SECRET_KEY = '%s'" %
       (''.join([choice('abcdefghijklmnopqrstuvwxyz0123456789' +
                        '!@#$%^&*(-_=+)') for i in range(50)])))

