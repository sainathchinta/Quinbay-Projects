package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.util.ArrayList;
import java.util.List;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@ToString
@Builder
@Table(name = OriginalSalesCategory.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {OriginalSalesCategory.OSC_CODE})})
public class OriginalSalesCategory extends GdnBaseEntity {

  private static final long serialVersionUID = 5560623458471442022L;

  public static final String TABLE_NAME = "PCC_ORIGINAL_SALES_CATEGORY";
  public static final String OSC_CODE = "OSC_CODE";
  public static final String OSC_SHORT_TEXT = "OSC_SHORT_TEXT";
  public static final String OSC_LONG_TEXT = "OSC_LONG_TEXT";
  public static final String COLUMN_ACTIVATED = "IS_ACTIVATED";

  @Column(name = OriginalSalesCategory.OSC_CODE, updatable = false)
  private String oscCode;

  @Column(name = OriginalSalesCategory.OSC_SHORT_TEXT)
  private String oscShortText;

  @Column(name = OriginalSalesCategory.OSC_LONG_TEXT)
  private String oscLongText;

  @Column(name = OriginalSalesCategory.COLUMN_ACTIVATED)
  private boolean activated = true;

  @ToString.Exclude
  @OneToMany(mappedBy = "originalSalesCategory", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<Category> masterCategories = new ArrayList<>();
}
