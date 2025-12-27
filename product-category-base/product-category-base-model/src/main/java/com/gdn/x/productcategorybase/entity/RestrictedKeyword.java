package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
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
@Table(name = RestrictedKeyword.TABLE_NAME)
public class RestrictedKeyword extends GdnBaseEntity {

  private static final long serialVersionUID = 3019044648395224415L;

  public static final String TABLE_NAME = "PCC_RESTRICTED_KEYWORD";
  private static final String COLUMN_KEYWORD = "KEYWORD";
  private static final String COLUMN_VALIDATE_ON_UI = "VALIDATE_ON_UI";
  private static final String COLUMN_VALIDATE_BY_DS = "VALIDATE_BY_DS";

  @Column(name = RestrictedKeyword.COLUMN_KEYWORD)
  private String keyword;

  @Column(name = RestrictedKeyword.COLUMN_VALIDATE_ON_UI)
  private Boolean validateOnUi;

  @Column(name = RestrictedKeyword.COLUMN_VALIDATE_BY_DS)
  private Boolean validateByDs;

}
