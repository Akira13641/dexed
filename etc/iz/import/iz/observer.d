/**
 * The iz implementation of the observer pattern.
 */
module iz.observer;

import
    iz.types, iz.memory, iz.containers;

/**
 * Subject (one to many) interface.
 */
interface Subject
{
    /// Determines if observer is suitable for this subject.
    bool acceptObserver(Object observer);
    /// An observer can be added after acceptObserver
    void addObserver(Object observer);
    /// an observer wants to be removed.
    void removeObserver(Object observer);
}

/**
 * CustomSubject handles a list of obsever.
 * Params:
 * OT = the observer type, either an interface or a class.
 */
class CustomSubject(OT): Subject
if (is(OT == interface) || is(OT == class))
{

protected:

    DynamicList!OT _observers;

public:

    ///
    this()
    {
        _observers = construct!(DynamicList!OT);
    }

    ~this()
    {
        destruct(_observers);
    }

    /// see the Subject interface.
    bool acceptObserver(Object observer)
    {
        return (cast(OT) observer !is null);
    }

    /// see the Subject interface.
    void addObserver(Object observer)
    {
        if (!acceptObserver(observer))
            return;
        OT obs = cast(OT) observer;
        if (_observers.find(obs) != -1)
            return;
        _observers.add(obs);
    }

    /// Calls addObserver() foreach object passed as argument.
    void addObservers(Objs...)(Objs objs)
    {
        foreach(obj; objs)
            addObserver(obj);
    }

    /// see the Subject interface.
    void removeObserver(Object observer)
    {
        if (auto obs = cast(OT) observer)
            _observers.remove(obs);
    }

    /// list of observers
    DynamicList!OT observers()
    {
        return _observers;
    }
}

/**
 * ObserverInterconnector is in charge for inter-connecting the subjects with
 * their observers, whatever are their specializations.
 *
 * With this class, an observer can connect to the right subjects
 * without having to know them. In the same fashion the subjects gets their
 * client list automatically filled.
 */
class ObserverInterconnector
{

private:

    DynamicList!Object _observers;
    DynamicList!Object _subjects;
    ptrdiff_t fUpdateCount;

public:

    ///
    this()
    {
        _observers = construct!(DynamicList!Object);
        _subjects = construct!(DynamicList!Object);
    }

    ~this()
    {
        _observers.destruct;
        _subjects.destruct;
    }

    /**
     * Several entities will be added.
     * This Avoids any superfluous update while adding.
     * Every beginUpdate() must be followed by an endUpdate().
     */
    void beginUpdate()
    {
        ++fUpdateCount;
    }

    /**
     * Several subjects or observers have been added.
     * Decrements a counter and update the entities if it's equal to 0.
     */
    void endUpdate()
    {
        --fUpdateCount;
        if (fUpdateCount > 0)
            return;
        updateAll;
    }

    /**
     * Adds an observer to the list.
     */
    void addObserver(Object observer)
    {
        if (_observers.find(observer) != -1)
            return;
        beginUpdate;
        _observers.add(observer);
        endUpdate;
    }

    /**
     * Adds a list of observer to the list.
     * Optimized for bulk adding.
     */
    void addObservers(Objs...)(Objs objs)
    {
        beginUpdate;
        foreach(obj; objs)
            addObserver(obj);
        endUpdate;
    }

    /**
     * Removes an observer from the list.
     */
    void removeObserver(Object observer)
    {
        beginUpdate;
        _observers.remove(observer);
        foreach(immutable i; 0 .. _subjects.count)
            (cast(Subject) _subjects[i]).removeObserver(observer);
        endUpdate;
    }

    /**
     * Adds a subject to the list.
     */
    void addSubject(Object subject)
    {
        if (_subjects.find(subject) != -1)
            return;
        if( (cast(Subject) subject) is null)
            return;
        beginUpdate;
        _subjects.add(subject);
        endUpdate;
    }

    /**
     * Adds several subjects to the list.
     * Optimized for bulk addition.
     */
    void addSubjects(Subjs...)(Subjs subjs)
    {
        beginUpdate;
        foreach(subj; subjs)
            addSubject(subj);
        endUpdate;
    }

    /**
     * Removes a subject from the entity list.
     */
    void removeSubject(Object subject)
    {
        beginUpdate;
        _subjects.remove(subject);
        endUpdate;
    }

    /**
     * Updates the connections between the entities stored in the global list.
     *
     * It has usually not be called manually.
     * During the process, each subject is visited by each observer.
     *
     * The complexity of the operation is usually reduced if beginUpdate()
     * and endUpdate() are used adequatly.
     */
    void updateObservers()
    {
        fUpdateCount = 0;
        foreach(immutable subjectIx; 0 .. _subjects.count)
        {
            Subject subject = cast(Subject) _subjects[subjectIx];
            foreach(immutable observerIx; 0 .. _observers.count)
                subject.addObserver(_observers[observerIx]);
        }
    }

    /// ditto
    alias updateAll = updateObservers;

}

unittest
{
    interface PropObserver(T)
    {
        void min(T aValue);
        void max(T aValue);
        void def(T aValue);
    }
    alias IntPropObserver = PropObserver!int;

    class Foo: IntPropObserver
    {
        int _min, _max, _def;
        void min(int aValue){_min = aValue;}
        void max(int aValue){_max = aValue;}
        void def(int aValue){_def = aValue;}
    }
    alias UintPropObserver = PropObserver!uint;

    class Bar: UintPropObserver
    {
        uint _min, _max, _def;
        void min(uint aValue){_min = aValue;}
        void max(uint aValue){_max = aValue;}
        void def(uint aValue){_def = aValue;}
    }

    class IntPropSubject : CustomSubject!IntPropObserver
    {
        int _min = int.min;
        int _max = int.max;
        int _def = int.init;
        void updateObservers()
        {
            for(auto i = 0; i < _observers.count; i++)
                (cast(IntPropObserver)_observers[i]).min(_min);
            for(auto i = 0; i < _observers.count; i++)
                (cast(IntPropObserver)_observers[i]).max(_max);
            for(auto i = 0; i < _observers.count; i++)
                (cast(IntPropObserver)_observers[i]).def(_def);
        }
    }

    class UintPropSubject : CustomSubject!UintPropObserver
    {
        uint _min = uint.min;
        uint _max = uint.max;
        uint _def = uint.init;
        void updateObservers()
        {
            for(auto i = 0; i < _observers.count; i++)
                (cast(UintPropObserver)_observers[i]).min(_min);
            for(auto i = 0; i < _observers.count; i++)
                (cast(UintPropObserver)_observers[i]).max(_max);
            for(auto i = 0; i < _observers.count; i++)
                (cast(UintPropObserver)_observers[i]).def(_def);
        }
    }

    auto nots1 = construct!Object;
    auto nots2 = construct!Object;
    auto inter = construct!ObserverInterconnector;
    auto isubj = construct!IntPropSubject;
    auto iobs1 = construct!Foo;
    auto iobs2 = construct!Foo;
    auto iobs3 = construct!Foo;
    auto usubj = construct!UintPropSubject;
    auto uobs1 = construct!Bar;
    auto uobs2 = construct!Bar;
    auto uobs3 = construct!Bar;

    scope(exit)
    {
        destructEach(inter, isubj, usubj);
        destructEach(iobs1, iobs2, iobs3);
        destructEach(uobs1, uobs2, uobs3);
        destructEach(nots1, nots2);
    }

    inter.beginUpdate;
    // add valid entities
    inter.addSubjects(isubj, usubj);
    inter.addObservers(iobs1, iobs2, iobs3);
    inter.addObservers(uobs1, uobs2, uobs3);
    // add invalid entities
    inter.addSubjects(nots1, nots2);
    inter.addObservers(nots1, nots2);
    // not added twice
    inter.addSubjects(isubj, usubj);
    inter.endUpdate;

    // check the subject and observers count
    assert(inter._subjects.count == 2);
    assert(inter._observers.count == 8);
    assert(isubj.observers.count == 3);
    assert(usubj.observers.count == 3);

    inter.beginUpdate;
    inter.removeObserver(iobs1);
    inter.endUpdate;

    assert(inter._subjects.count == 2);
    assert(inter._observers.count == 7);
    assert(isubj._observers.count == 2);
    assert(usubj._observers.count == 3);

    // update subject
    isubj._min = -127;
    isubj._max = 128;
    isubj.updateObservers;
    // iobs1 has been removed
    assert(iobs1._min != -127);
    assert(iobs1._max != 128);
    // check observers
    assert(iobs2._min == -127);
    assert(iobs2._max == 128);
    assert(iobs3._min == -127);
    assert(iobs3._max == 128);

    // update subject
    usubj._min = 2;
    usubj._max = 256;
    usubj.updateObservers;
    // check observers
    assert(uobs1._min == 2);
    assert(uobs1._max == 256);
    assert(uobs2._min == 2);
    assert(uobs2._max == 256);
    assert(uobs3._min == 2);
    assert(uobs3._max == 256);
}



/**
 * interface for an observer based on an enum.
 * Params:
 * E = an enum.
 * T = variadic type of the parameters an observer monitors.
 */
interface EnumBasedObserver(E, T...)
if (is(E == enum))
{
    /**
     * The method called by a subject.
     * Params:
     * notification = the E member allowing to distinguish the "call reason".
     * t = the parameters the observer is interested by.
     */
    void subjectNotification(E notification, T t);   
}

/**
 * CustomSubject handles a list of obsever.
 * This version only accept an observer if it's an EnumBasedObserver.
 * Params:
 * E = an enum.
 * T = the variadic list of parameter types used in the notification. 
 */
class CustomSubject(E, T...) : Subject 
if (is(E == enum))
{

protected:

    alias ObserverType = EnumBasedObserver!(E,T);
    DynamicList!ObserverType _observers;

public:

    ///
    this()
    {
        _observers = construct!(DynamicList!ObserverType);
    }

    ~this()
    {
        _observers.destruct;
    }

    /// see the Subject interface.
    bool acceptObserver(Object observer)
    in
    {
        assert(observer);
    }
    body
    {
        return (cast(ObserverType) observer !is null);
    }

    /// see the Subject interface.
    void addObserver(Object observer)
    in
    {
        assert(observer);
    }
    body
    {
        if (!acceptObserver(observer))
            return;
        auto obs = cast(ObserverType) observer;
        if (_observers.find(obs) != -1)
            return;
        _observers.add(obs);
    }

    /// Calls addObserver() foreach object passed as argument.
    void addObservers(Objs...)(Objs objs)
    {
        foreach(obj; objs)
            addObserver(obj);
    }

    /// see the Subject interface.
    void removeObserver(Object observer)
    in
    {
        assert(observer);
    }
    body
    {
        auto obs = cast(ObserverType) observer;
        _observers.remove(obs);
    }
    ///
    DynamicList!ObserverType observers()
    {
        return _observers;
    }
} 

unittest
{
    enum DocumentNotification{opening, closing, saving, changed}
    
    class Document {}
    class Highlighter {}
    class Invalid {}
    
    class DocSubject : CustomSubject!(DocumentNotification, Document, Highlighter)
    {
        void notify(DocumentNotification dn)
        {
            foreach(obs;_observers)
                (cast(ObserverType) obs)
                    .subjectNotification(dn, null, null);
        }
    }
    class DocObserver: EnumBasedObserver!(DocumentNotification, Document, Highlighter)
    {
        DocumentNotification lastNotification;
        void subjectNotification(DocumentNotification notification, Document doc, Highlighter hl)
        {
            lastNotification = notification;
        }
    }   
    
    ObserverInterconnector inter = construct!ObserverInterconnector;
    DocSubject subj = construct!DocSubject;
    DocObserver obs1 = construct!DocObserver;
    DocObserver obs2 = construct!DocObserver;
    DocObserver obs3 = construct!DocObserver;
    
    scope(exit) destructEach(inter, subj, obs1, obs2, obs3);
    
    inter.addSubject(subj);
    inter.addObservers(obs1, obs2, obs3);
    inter.addObservers(obs1, obs2, obs3);
    assert(inter._observers.count == 3);
    assert(subj._observers.count == 3);
    
    subj.notify(DocumentNotification.changed);
    assert(obs1.lastNotification == DocumentNotification.changed);
    assert(obs2.lastNotification == DocumentNotification.changed);
    assert(obs3.lastNotification == DocumentNotification.changed);
    subj.notify(DocumentNotification.opening);
    assert(obs1.lastNotification == DocumentNotification.opening);
    assert(obs2.lastNotification == DocumentNotification.opening);
    assert(obs3.lastNotification == DocumentNotification.opening);

    inter.removeSubject(subj);
    assert(inter._subjects.count == 0);
    assert(inter._observers.count == 3);
    assert(subj.observers.count == 3);

    inter.removeObserver(obs2);
    assert(inter._observers.count == 2);
    assert(subj.observers.count == 3);
    subj.removeObserver(obs2);
    assert(subj.observers.count == 2);

    auto inv = construct!Invalid;
    inter.addSubject(subj);
    inter.addObserver(inv);
    assert(inter._observers.count == 3);
    assert(subj.observers.count == 2);
    destruct(inv);
}

