package com.gdn.x.productcategorybase.csv;

import org.supercsv.cellprocessor.ParseEnum;
import org.supercsv.cellprocessor.constraint.NotNull;
import org.supercsv.cellprocessor.ift.CellProcessor;

import com.gdn.x.productcategorybase.CatalogType;

public class CatalogCsv {

  public static final String[] INPUT_HEADER = new String[] {"storeId", "catalogType", "name"};
  public static final CellProcessor[] INPUT_PROCESSORS =
      new CellProcessor[] {new NotNull(), new ParseEnum(CatalogType.class), new NotNull()};
  public static final String[] ACTUAL_HEADER = new String[] {"Store Id", "Catalog Type", "Catalog Name", "Catalog Id"};
  public static final String[] OUTPUT_HEADER = new String[] {"storeId", "catalogType", "name", "id"};
  public static final CellProcessor[] OUTPUT_PROCESSORS =
      new CellProcessor[] {new NotNull(), new NotNull(), new NotNull(), new NotNull()};

  private String name;
  private CatalogType catalogType;
  private String storeId;
  private String id;

  public CatalogCsv() {
    // nothing to do here
  }

  public CatalogCsv(String name, CatalogType catalogType, String storeId) {
    this.name = name;
    this.catalogType = catalogType;
  }

  public CatalogType getCatalogType() {
    return this.catalogType;
  }

  public String getId() {
    return this.id;
  }

  public String getName() {
    return this.name;
  }

  public String getStoreId() {
    return this.storeId;
  }

  public void setCatalogType(CatalogType catalogType) {
    this.catalogType = catalogType;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  @Override
  public String toString() {
    return String.format("Catalog [name=%s, catalogType=%s, toString()=%s]", this.name,
        this.getCatalogType(), super.toString());
  }

}
