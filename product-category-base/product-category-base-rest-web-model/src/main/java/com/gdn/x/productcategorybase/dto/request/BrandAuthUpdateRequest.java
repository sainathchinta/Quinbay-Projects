package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthUpdateRequest implements Serializable {
  private static final long serialVersionUID = 9028100048885000869L;
  private String sellerCode;
  private String brandCode;
  private String brandName;
  private String authorisationStatus;
  private Date authStartDate;
  private Date authExpireDate;
  private List<String> documentLinks;
  private String iprRegistrationNumber;
}
