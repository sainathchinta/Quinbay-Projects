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
public class BrandAuthCreateWipRequest implements Serializable {

  private static final long serialVersionUID = -492339941874487488L;
  private String brandName;
  private String brandCode;
  private String sellerCode;
  private Date authStartDate;
  private Date authExpireDate;
  private List<String> documentLinks;
  private String iprRegistrationNumber;
}
