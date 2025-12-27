package com.gdn.mta.bulk.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.EqualsAndHashCode;

@Entity
@EqualsAndHashCode(callSuper=true)
@Table(name = BulkProcessNotes.TABLE_NAME)
public class BulkProcessNotes extends GdnBaseEntity {

  private static final long serialVersionUID = -7691105044581529143L;
  public static final String TABLE_NAME = "BLP_BULK_PROCESS_NOTES";
  public static final String COLUMN_BULK_PROCESS_CODE = "BULK_PROCESS_CODE";
  public static final String COLUMN_NOTES = "NOTES";
  public static final String COLUMN_BULK_PROCESS_ID = "BULK_PROCESS_ID";
  public static final String COLUMN_IS_PROMO_NOTE = "IS_PROMO_NOTE";
  public static final String COLUMN_IS_WHOLESALE_CONFIG = "IS_WHOLESALE_CONFIG";

  @Column(name = COLUMN_BULK_PROCESS_CODE, nullable = false)
  private String bulkProcessCode;

  @Column(name = COLUMN_NOTES)
  private String notes;

  @Column(name = COLUMN_IS_PROMO_NOTE)
  private boolean isPromoNote = false;

  @ManyToOne
  @JoinColumn(name = COLUMN_BULK_PROCESS_ID)
  private BulkProcess bulkProcess;

  @Column(name = COLUMN_IS_WHOLESALE_CONFIG)
  private boolean wholeSaleConfig = false;

  public BulkProcessNotes() {
  }

  public BulkProcessNotes(String bulkProcessCode, String notes, BulkProcess bulkProcess) {
    super();
    this.bulkProcessCode = bulkProcessCode;
    this.notes = notes;
    this.bulkProcess = bulkProcess;
  }

  public BulkProcessNotes(String bulkProcessCode, String notes, boolean isPromoNote, BulkProcess bulkProcess) {
    super();
    this.bulkProcessCode = bulkProcessCode;
    this.notes = notes;
    this.isPromoNote = isPromoNote;
    this.bulkProcess = bulkProcess;
  }

  public boolean isPromoNote() {
    return isPromoNote;
  }

  public void setPromoNote(boolean promoNote) {
    isPromoNote = promoNote;
  }

  public BulkProcess getBulkProcess() {
    return bulkProcess;
  }

  public String getBulkProcessCode() {
    return bulkProcessCode;
  }

  public String getNotes() {
    return notes;
  }

  public void setBulkProcess(BulkProcess bulkProcess) {
    this.bulkProcess = bulkProcess;
  }

  public void setBulkProcessCode(String bulkProcessCode) {
    this.bulkProcessCode = bulkProcessCode;
  }

  public void setNotes(String notes) {
    if(this.notes != null)
        this.notes += notes;
    else
      this.notes = notes;
  }

  public boolean isWholeSaleConfig() {
    return wholeSaleConfig;
  }

  public void setWholeSaleConfig(boolean wholeSaleConfig) {
    this.wholeSaleConfig = wholeSaleConfig;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkProcessNotes{");
    sb.append("bulkProcessCode='").append(bulkProcessCode).append('\'');
    sb.append(", notes='").append(notes).append('\'');
    sb.append(", isPromoNote=").append(isPromoNote);
    sb.append(", bulkProcess=").append(bulkProcess);
    sb.append(", wholeSaleConfig").append(wholeSaleConfig);
    sb.append('}');
    return sb.toString();
  }
}
