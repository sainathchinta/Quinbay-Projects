package com.gdn.x.productcategorybase.entity.brand;

import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Date;

@Entity
@Table(name = BrandAuthorisation.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {BrandAuthorisation.COLUMN_SELLER_CODE, BrandAuthorisation.COLUMN_BRAND_CODE})})
@EqualsAndHashCode(callSuper=true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class BrandAuthorisation extends GdnBaseEntity {

  public static final String TABLE_NAME = "PCC_BRAND_AUTHORISATION";
  public static final String COLUMN_BRAND_CODE = "BRAND_CODE";
  public static final String COLUMN_BRAND_NAME = "BRAND_NAME";
  public static final String COLUMN_SELLER_CODE = "SELLER_CODE";
  public static final String COLUMN_AUTHORISATION_STATUS = "AUTHORISATION_STATUS";
  public static final String COLUMN_AUTH_START_DATE = "AUTH_START_DATE";
  public static final String COLUMN_AUTH_EXPIRE_DATE = "AUTH_EXPIRE_DATE";
  public static final String COLUMN_DOCUMENT_LINK = "DOCUMENT_LINK";

  @Column(name = BrandAuthorisation.COLUMN_BRAND_CODE, nullable = false)
  private String brandCode;

  @Column(name = BrandAuthorisation.COLUMN_BRAND_NAME, nullable = false)
  private String brandName;

  @Column(name = BrandAuthorisation.COLUMN_SELLER_CODE, nullable = false)
  private String sellerCode;

  @Column(name = BrandAuthorisation.COLUMN_AUTHORISATION_STATUS)
  @Enumerated(EnumType.STRING)
  private BrandAuthorisationStatus authorisationStatus;

  @Column(name = BrandAuthorisation.COLUMN_AUTH_START_DATE)
  private Date authStartDate;

  @Column(name = BrandAuthorisation.COLUMN_AUTH_EXPIRE_DATE)
  private Date authExpireDate;

  @Column(name = BrandAuthorisation.COLUMN_DOCUMENT_LINK)
  private String documentLink;
}
