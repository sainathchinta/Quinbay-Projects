package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@EqualsAndHashCode(callSuper=true)
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = CategoryRestrictedKeyword.TABLE_NAME)
public class CategoryRestrictedKeyword extends GdnBaseEntity {

  private static final long serialVersionUID = 4675360811474734168L;

  public static final String TABLE_NAME = "PCC_CATEGORY_RESTRICTED_KEYWORD";
  public static final String COLUMN_CATEGORY_ID = "CATEGORY_ID";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  public static final String COLUMN_KEYWORD_ID = "KEYWORD_ID";
  public static final String COLUMN_TYPE = "TYPE";
  public static final String COLUMN_ACTION = "ACTION";
  public static final String COLUMN_MESSAGE = "MESSAGE";
  public static final String COLUMN_DESTINATION_CATEGORY = "DESTINATION_CATEGORY";

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = CategoryRestrictedKeyword.COLUMN_CATEGORY_ID)
  private Category category;

  @Column(name = CategoryRestrictedKeyword.COLUMN_CATEGORY_ID, insertable = false, updatable = false)
  private String categoryId;

  @Column(name = CategoryRestrictedKeyword.COLUMN_CATEGORY_CODE)
  private String categoryCode;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = CategoryRestrictedKeyword.COLUMN_KEYWORD_ID)
  private RestrictedKeyword restrictedKeyword;

  @Column(name = CategoryRestrictedKeyword.COLUMN_KEYWORD_ID, insertable = false, updatable = false)
  private String restrictedKeywordId;

  @Column(name = CategoryRestrictedKeyword.COLUMN_TYPE)
  private String type;

  @Column(name = CategoryRestrictedKeyword.COLUMN_ACTION)
  private int action;

  @Column(name = CategoryRestrictedKeyword.COLUMN_MESSAGE)
  private String message;

  @Column(name = CategoryRestrictedKeyword.COLUMN_DESTINATION_CATEGORY)
  private String destinationCategory;

}
