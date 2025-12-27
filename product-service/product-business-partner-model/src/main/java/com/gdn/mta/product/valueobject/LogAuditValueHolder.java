package com.gdn.mta.product.valueobject;

public class LogAuditValueHolder<T> {
  T oldValue;
  T newValue;
  
  public LogAuditValueHolder(T oldValue, T newValue) {
    this.oldValue = oldValue;
    this.newValue = newValue;
  }

  public T getOldValue() {
    return oldValue;
  }

  public void setOldValue(T oldValue) {
    this.oldValue = oldValue;
  }

  public T getNewValue() {
    return newValue;
  }

  public void setNewValue(T newValue) {
    this.newValue = newValue;
  }
}
