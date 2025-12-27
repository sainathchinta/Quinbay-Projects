package com.gdn.partners.pdt.entity.sequence;

import java.io.Serializable;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

@Entity
@Table(name = Sequence.TABLE_NAME)
public class Sequence implements Serializable {

  private static final long serialVersionUID = -4875386088862457058L;
  public static final String TABLE_NAME = "PDT_SEQUENCE";
  public static final String COLUMN_KEY = "KEY";
  public static final String COLUMN_COUNTER = "COUNTER";
  public static final String COLUMN_INCREMENT = "INCREMENT";

  @Id
  @org.springframework.data.annotation.Id
  @Column(name = Sequence.COLUMN_KEY)
  private String key;

  @Column(name = Sequence.COLUMN_COUNTER, columnDefinition = "INTEGER DEFAULT 0")
  private Integer counter;

  @Column(name = Sequence.COLUMN_INCREMENT, columnDefinition = "INTEGER DEFAULT 1")
  private Integer increment;

  public Sequence() {}

  public Sequence(String key, Integer counter, Integer increment) {
    super();
    this.key = key;
    this.counter = counter;
    this.increment = increment;
  }

  public String getKey() {
    return key;
  }

  public void setKey(String key) {
    this.key = key;
  }

  public Integer getCounter() {
    return counter;
  }

  public void setCounter(Integer counter) {
    this.counter = counter;
  }

  public Integer getIncrement() {
    return increment;
  }

  public void setIncrement(Integer increment) {
    this.increment = increment;
  }

  @Override
  public String toString() {
    return String.format("Sequence [key=%s, counter=%s, increment=%s]", key, counter, increment);
  }

}
