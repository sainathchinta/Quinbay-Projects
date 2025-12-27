package com.gdn.x.productcategorybase.entity.brand;

import com.gdn.x.productcategorybase.constants.FieldNames;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@EqualsAndHashCode(callSuper=true)
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = BrandWipHistory.TABLE_NAME)
public class BrandWipHistory extends GdnBaseEntity {


  private static final long serialVersionUID = 6514347506121084108L;
  public static final String TABLE_NAME = "PCC_BRAND_WIP_HISTORY";

  @Column(name = FieldNames.COLUMN_BRAND_REQUEST_CODE)
  private String brandRequestCode;

  @Column(name = FieldNames.COLUMN_BRAND_CODE)
  private String brandCode;

  @Column(name = FieldNames.COLUMN_DESCRIPTION, nullable = false)
  private byte[] description;

  @Enumerated(value = EnumType.STRING)
  @Column(name = FieldNames.COLUMN_STATE, nullable = false)
  private BrandWipState state;

}
