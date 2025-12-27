package com.gdn.x.productcategorybase.dto.brand;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown=true)
public class BrandAuthCreateRequest implements Serializable {

  private static final long serialVersionUID = 7652911430592054779L;
  private String brandName;
  private String brandCode;
  private String sellerCode;
  private String authorisationStatus;
  private Date authStartDate;
  private Date authExpireDate;
  private List<String> documentLinks;
  private boolean bulkAction;
}
