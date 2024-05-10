#include <stdlib.h>
#include <Python.h>
#include "python_lib_wrapper.h"

long long factorial_iterative_python (int n) {

    // Komentarze tylko w pierwszej funkcji - kolejne są analogiczne
    long long result;
    // Inicjalizacja interpretera Pythona
    Py_Initialize();

    // Dodanie bieżącego katalogu do ścieżki poszukiwania modułów
    PyObject *sys = PyImport_ImportModule("sys");
    PyObject *path = PyObject_GetAttrString(sys, "path");
    PyList_Append(path, PyUnicode_FromString("./python_library"));

    // Importowanie modułu Pythona
    PyObject* pModule = PyImport_ImportModule("iterative_lib");

    if (pModule != NULL) {
        // Pobranie referencji do funkcji "add" z modułu
        PyObject* pFunc = PyObject_GetAttrString(pModule, "factorial_iterative");

        if (pFunc && PyCallable_Check(pFunc)) {
            // Wywołanie funkcji
            PyObject* pArgs = PyTuple_New(1); // Określenie liczby argumentów
            PyTuple_SetItem(pArgs, 0, PyLong_FromLong(n)); // Dodawanie kolejnych argumentów

            PyObject* pResult = PyObject_CallObject(pFunc, pArgs);

            if (pResult != NULL) {
                // Pobranie wyniku z funkcji i wyświetlenie go
                result = PyLong_AsLong(pResult);
                Py_DECREF(pResult);
            }
            Py_XDECREF(pArgs);
        }
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
    }
    else {
        // Obsługa błędów importowania modułu
        PyErr_Print();
    }

    // Wyłączenie interpretera Pythona
    Py_Finalize();

    return result; 
}

unsigned int gcd_iterative_python (unsigned int n, unsigned int m) {

    unsigned int result;

    Py_Initialize();
    PyObject *sys = PyImport_ImportModule("sys");
    PyObject *path = PyObject_GetAttrString(sys, "path");
    PyList_Append(path, PyUnicode_FromString("./python_library"));
    PyObject* pModule = PyImport_ImportModule("iterative_lib");

    if (pModule != NULL) {
        PyObject* pFunc = PyObject_GetAttrString(pModule, "gcd_iterative");
        if (pFunc && PyCallable_Check(pFunc)) {
            PyObject* pArgs = PyTuple_New(2);
            PyTuple_SetItem(pArgs, 0, PyLong_FromLong(n));
            PyTuple_SetItem(pArgs, 1, PyLong_FromLong(m));
            PyObject* pResult = PyObject_CallObject(pFunc, pArgs);
            if (pResult != NULL) {
                result = PyLong_AsLong(pResult);
                Py_DECREF(pResult);
            }
            Py_XDECREF(pArgs);
        }
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
    }
    else {
        PyErr_Print();
    }

    Py_Finalize();

    return result;
}

Solution diophantine_iterative_python (int a, int b, int c) {
    
    Solution result;

    Py_Initialize();
    PyObject *sys = PyImport_ImportModule("sys");
    PyObject *path = PyObject_GetAttrString(sys, "path");
    PyList_Append(path, PyUnicode_FromString("./python_library"));
    PyObject* pModule = PyImport_ImportModule("iterative_lib");

    if (pModule != NULL) {
        PyObject* pFunc = PyObject_GetAttrString(pModule, "diophantine_iterative");
        if (pFunc && PyCallable_Check(pFunc)) {
            PyObject* pArgs = PyTuple_New(3);
            PyTuple_SetItem(pArgs, 0, PyLong_FromLong(a));
            PyTuple_SetItem(pArgs, 1, PyLong_FromLong(b));
            PyTuple_SetItem(pArgs, 2, PyLong_FromLong(c));
            PyObject* pResult = PyObject_CallObject(pFunc, pArgs);
            if (pResult != NULL) {
                PyObject* pX = PyObject_GetAttrString(pResult, "x");
                result.x = PyLong_AsLong(pX);
                Py_DECREF(pX);
                PyObject* pY = PyObject_GetAttrString(pResult, "y");
                result.y = PyLong_AsLong(pY);
                Py_DECREF(pY);
                PyObject* pErr = PyObject_GetAttrString(pResult, "err");
                result.err = PyObject_IsTrue(pErr);
                Py_DECREF(pErr);
            }
            Py_XDECREF(pArgs);
        }
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
    }
    else {
        PyErr_Print();
    }

    Py_Finalize();

    return result;
}

long long factorial_recursive_python (int n) {

    long long result;

    Py_Initialize();
    PyObject *sys = PyImport_ImportModule("sys");
    PyObject *path = PyObject_GetAttrString(sys, "path");
    PyList_Append(path, PyUnicode_FromString("./python_library"));
    PyObject* pModule = PyImport_ImportModule("recursive_lib");

    if (pModule != NULL) {
        PyObject* pFunc = PyObject_GetAttrString(pModule, "factorial_recursive");
        if (pFunc && PyCallable_Check(pFunc)) {
            PyObject* pArgs = PyTuple_New(1);
            PyTuple_SetItem(pArgs, 0, PyLong_FromLong(n));
            PyObject* pResult = PyObject_CallObject(pFunc, pArgs);
            if (pResult != NULL) {
                result = PyLong_AsLong(pResult);
                Py_DECREF(pResult);
            }
            Py_XDECREF(pArgs);
        }
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
    }
    else {
        PyErr_Print();
    }

    Py_Finalize();

    return result;    
}

unsigned int gcd_recursive_python (unsigned int n, unsigned int m) {

    unsigned int result;

    Py_Initialize();
    PyObject *sys = PyImport_ImportModule("sys");
    PyObject *path = PyObject_GetAttrString(sys, "path");
    PyList_Append(path, PyUnicode_FromString("./python_library"));
    PyObject* pModule = PyImport_ImportModule("recursive_lib");

    if (pModule != NULL) {
        PyObject* pFunc = PyObject_GetAttrString(pModule, "gcd_recursive");
        if (pFunc && PyCallable_Check(pFunc)) {
            PyObject* pArgs = PyTuple_New(2);
            PyTuple_SetItem(pArgs, 0, PyLong_FromLong(n));
            PyTuple_SetItem(pArgs, 1, PyLong_FromLong(m));
            PyObject* pResult = PyObject_CallObject(pFunc, pArgs);
            if (pResult != NULL) {
                result = PyLong_AsLong(pResult);
                Py_DECREF(pResult);
            }
            Py_XDECREF(pArgs);
        }
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
    }
    else {
        PyErr_Print();
    }

    Py_Finalize();

    return result;    
}

Solution diophantine_recursive_python (int a, int b, int c) {

    Solution result;

    Py_Initialize();
    PyObject *sys = PyImport_ImportModule("sys");
    PyObject *path = PyObject_GetAttrString(sys, "path");
    PyList_Append(path, PyUnicode_FromString("./python_library"));
    PyObject* pModule = PyImport_ImportModule("recursive_lib");
    if (pModule != NULL) {
        PyObject* pFunc = PyObject_GetAttrString(pModule, "diophantine_recursive");
        if (pFunc && PyCallable_Check(pFunc)) {
            PyObject* pArgs = PyTuple_New(3);
            PyTuple_SetItem(pArgs, 0, PyLong_FromLong(a));
            PyTuple_SetItem(pArgs, 1, PyLong_FromLong(b));
            PyTuple_SetItem(pArgs, 2, PyLong_FromLong(c));
            PyObject* pResult = PyObject_CallObject(pFunc, pArgs);
            if (pResult != NULL) {
                PyObject* pX = PyObject_GetAttrString(pResult, "x");
                result.x = PyLong_AsLong(pX);
                Py_DECREF(pX);
                PyObject* pY = PyObject_GetAttrString(pResult, "y");
                result.y = PyLong_AsLong(pY);
                Py_DECREF(pY);
                PyObject* pErr = PyObject_GetAttrString(pResult, "err");
                result.err = PyObject_IsTrue(pErr);
                Py_DECREF(pErr);
            }
            Py_XDECREF(pArgs);
        }
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
    }
    else {
        PyErr_Print();
    }

    Py_Finalize();

    return result;    
}
