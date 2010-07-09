from django.db import models


class Location(models.Model):
    street = models.CharField(max_length=256)
    city = models.CharField(max_length=100)
    state = models.CharField(max_length=100)

    def toJson(self, deep=False):
        return {'id': self.id,
                'street': self.street,
                'city': self.city,
                'state': self.state}

    @classmethod
    def fromJson(cls, json):
        return Location.objects.create(
            id=json['id'],
            street=json['street'],
            city=json['city'],
            state=json['state'])


class Person(models.Model):
    name = models.CharField(max_length=100)
    home = models.ForeignKey(Location, null=True)

    def toJson(self, deep=False):
        json = {'id': self.id,
                'name': self.name}
        if deep:
            json['home'] = self.home and self.home.toJson()
        else:
            json['home_id'] = self.home_id
        return json

    @classmethod
    def fromJson(cls, json):
        return Person.objects.create(
            id=json['id'],
            name=json['name'],
            home_id=json['home_id'])


def clear():
    [x.delete() for x in Location.objects.all()]
    [x.delete() for x in Person.objects.all()]

def save():
    outf = open('ZZ', 'w')
    for model in (Location, Person):
        outf.write(repr([x.toJson() for x in model.objects.all()]))
        outf.write('\n')
    outf.close()

def load():
    inf = open('ZZ')
    for model in (Location, Person):
        L = inf.readline()
        for x in eval(L):
            obj = model.fromJson(x)
            obj.save()
