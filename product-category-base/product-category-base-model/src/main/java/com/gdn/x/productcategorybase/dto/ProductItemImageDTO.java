package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import com.gdn.x.productcategorybase.entity.ProductItem;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemImageDTO extends GdnBaseEntity {

  private static final long serialVersionUID = 1305232153493458262L;
  private String productItemId;
  private boolean isMainImages;
  private String locationPath;
  private Integer sequence;
  private boolean active;
  private String hashCode;
  private Boolean originalImage;
  private boolean edited;
  private boolean revised;
  private boolean commonImage;
  private String storeId;
  private Long version;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private boolean markForDelete;
}
