package com.gdn.x.productcategorybase.entity;

import com.gdn.x.productcategorybase.CatalogType;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = Catalog.TABLE_NAME)
public class Catalog extends GdnBaseEntity {

  private static final long serialVersionUID = 1117194818272320856L;
  public static final String TABLE_NAME = "PCC_CATALOG";
  public static final String COLUMN_CATALOG_CODE = "CATALOG_CODE";
  public static final String COLUMN_CATALOG_NAME = "NAME";
  public static final String COLUMN_CATALOG_TYPE = "CATALOG_TYPE";

  @Column(name = Catalog.COLUMN_CATALOG_CODE)
  private String catalogCode;

  @Column(name = Catalog.COLUMN_CATALOG_NAME)
  private String name;

  @Enumerated(EnumType.STRING)
  @Column(name = Catalog.COLUMN_CATALOG_TYPE)
  private CatalogType catalogType;

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "catalog", fetch = FetchType.LAZY)
  private List<Category> categories = new ArrayList<Category>();

  public Catalog() {
    // nothing to do here
  }

  public Catalog(String id) {
    this.setId(id);
  }

  public Catalog(String name, String catalogCode, CatalogType catalogType, String storeId) {
    this.name = name;
    this.catalogCode = catalogCode;
    this.catalogType = catalogType;
    this.setStoreId(storeId);
  }

  public String getCatalogCode() {
    return this.catalogCode;
  }

  public CatalogType getCatalogType() {
    return this.catalogType;
  }

  public List<Category> getCategories() {
    return this.categories;
  }

  public String getName() {
    return this.name;
  }

  public void setCatalogCode(String catalogCode) {
    this.catalogCode = catalogCode;
  }

  public void setCatalogType(CatalogType catalogType) {
    this.catalogType = catalogType;
  }

  public void setCategories(List<Category> categories) {
    this.categories = categories;
  }

  public void setName(String name) {
    this.name = name;
  }

  @Override
  public String toString() {
    return String.format("Catalog [name=%s, catalogCode=%s, catalogType=%s, toString()=%s]", this.name,
        this.getCatalogCode(), this.getCatalogType(), super.toString());
  }

}
