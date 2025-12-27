package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import org.apache.commons.lang3.builder.ToStringBuilder;

import com.gdn.GdnBaseEntity;

/**
 * Created by hardikbohra on 10/05/18.
 */

@Entity
@Table(name = Recategorization.TABLE_NAME)
public class Recategorization extends GdnBaseEntity {

  public static final String TABLE_NAME = "PRD_RECATEGORIZATION";
  private static final String COLUMN_NAME = "NAME";
  private static final String COLUMN_STATUS = "STATUS";
  private static final String COLUMN_EXCEL_PATH = "EXCEL_PATH";
  private static final String COLUMN_STORE_ID = "STORE_ID";

  @Column(name = COLUMN_NAME, nullable = false)
  private String name;

  @Column(name = COLUMN_STATUS, nullable = false)
  private String status;

  @Column(name = COLUMN_EXCEL_PATH, nullable = false)
  private String excelFilePath;

  public Recategorization() {
    // default constructor
  }

  public Recategorization(String name, String status, String excelFilePath) {
    this.name = name;
    this.status = status;
    this.excelFilePath = excelFilePath;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String isStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  public String getExcelFilePath() {
    return excelFilePath;
  }

  public void setExcelFilePath(String excelFilePath) {
    this.excelFilePath = excelFilePath;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("name", name).append("status", status).append("excelFilePath",
        excelFilePath).toString();
  }
}
