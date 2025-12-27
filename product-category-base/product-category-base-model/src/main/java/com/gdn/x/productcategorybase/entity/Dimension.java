package com.gdn.x.productcategorybase.entity;

import com.gdn.x.productcategorybase.AttributeType;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
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

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Builder
@Table(name = Dimension.TABLE_NAME, uniqueConstraints = {
  @UniqueConstraint(columnNames = {Dimension.COLUMN_DIMENSION_CODE,
    Dimension.COLUMN_DIMENSION_NAME})})
@ToString()
public class Dimension extends GdnBaseEntity {

  private static final long serialVersionUID = 5515200663836229195L;

  public static final String TABLE_NAME = "PCC_DIMENSION";
  public static final String COLUMN_DIMENSION_NAME = "NAME";
  public static final String COLUMN_DIMENSION_CODE = "DIMENSION_CODE";
  public static final String COLUMN_DIMENSION_TYPE = "DIMENSION_TYPE";
  public static final String COLUMN_DIMENSION_VALUE_TYPE = "VALUE_TYPE";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  public static final String COLUMN_EXAMPLE = "EXAMPLE";
  public static final String COLUMN_NAME_ENGLISH = "NAME_ENGLISH";
  public static final String COLUMN_DESCRIPTION_ENGLISH = "DESCRIPTION_ENGLISH";
  public static final String COLUMN_DESCRIPTION_SEARCH = "DESCRIPTION_SEARCH";
  public static final String COLUMN_DS_ATTRIBUTE_NAME = "DS_ATTRIBUTE_NAME";

  @Column(name = Dimension.COLUMN_DIMENSION_NAME)
  private String name;

  @Column(name = Dimension.COLUMN_DIMENSION_CODE)
  private String dimensionCode;

  @Enumerated(EnumType.STRING)
  @Column(name = Dimension.COLUMN_DIMENSION_TYPE)
  private AttributeType dimensionType;

  @OneToMany(mappedBy = "dimension", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  @ToString.Exclude
  private List<DimensionMapping> dimensionMappings = new ArrayList<>();

  @Column(name = Dimension.COLUMN_DESCRIPTION)
  private byte[] description;

  @Column(name = Dimension.COLUMN_EXAMPLE)
  private String example;

  @Column(name = Dimension.COLUMN_NAME_ENGLISH)
  private String nameEnglish;

  @Column(name = Dimension.COLUMN_DESCRIPTION_ENGLISH)
  private byte[] descriptionEnglish;

  @Column(name = Dimension.COLUMN_DESCRIPTION_SEARCH)
  private String descriptionSearch;

  @Column(name = Dimension.COLUMN_DS_ATTRIBUTE_NAME)
  private String dsAttributeName;
}
