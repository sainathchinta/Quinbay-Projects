package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.partners.core.web.dto.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class VendorDetailWebResponse extends BaseResponse {


  private static final long serialVersionUID = 7285861937454065443L;

  private String name;
  private String vendorCode;
  private Integer slaInDays;
  private Boolean isAbleToReject;
  private Boolean isQcRequired;
  private Date startHolidayDate;
  private Date endHolidayDate;
  private Integer quota;
  private String description;
  private String id;
  private Integer remainingCapacity;
}
