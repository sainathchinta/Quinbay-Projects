package com.gdn.mta.product.entity;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

@Entity
@Table(name = Sequence.TABLE_NAME)
public class Sequence implements Serializable {

  private static final long serialVersionUID = 5742785903339399612L;
  public static final String TABLE_NAME = "PRD_SEQUENCE";
  public static final String COLUMN_KEY = "KEY";
  public static final String COLUMN_COUNTER = "COUNTER";
  public static final String COLUMN_INCREMENT = "INCREMENT";

  @Id
  @Column(name = COLUMN_KEY)
  @org.springframework.data.annotation.Id
  private String key;

  @Column(name = COLUMN_COUNTER, columnDefinition = "INTEGER DEFAULT 0")
  private Integer counter;

  @Column(name = COLUMN_INCREMENT, columnDefinition = "INTEGER DEFAULT 1")
  private Integer increment;

  public Sequence() {}

  public Sequence(String key, Integer counter, Integer increment) {
    super();
    this.key = key;
    this.counter = counter;
    this.increment = increment;
  }

  public Integer getCounter() {
    return counter;
  }

  public Integer getIncrement() {
    return increment;
  }

  public String getKey() {
    return key;
  }

  public void setCounter(Integer counter) {
    this.counter = counter;
  }

  public void setIncrement(Integer increment) {
    this.increment = increment;
  }

  public void setKey(String key) {
    this.key = key;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("Sequence [key=").append(key).append(", counter=").append(counter).append(", increment=")
    .append(increment).append(", getClass()=").append(getClass()).append(", hashCode()=").append(hashCode())
    .append(", toString()=").append(super.toString()).append("]");
    return builder.toString();
  }

}
