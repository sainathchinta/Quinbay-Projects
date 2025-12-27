package com.gdn.partners.pcu.internal.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthorisationDetailWebResponse extends BaseWebResponse{
  private static final long serialVersionUID = 2098007503147384130L;
  private String brandName;
  private String sellerId;
  private Date authStartDate;
  private Date authExpireDate;
  private BrandAuthorisationStatus authorisationStatus;
  private String sellerName;
}
