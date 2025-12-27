package com.gdn.partners.pcu.external.web.model.response;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MasterWarehouseListWebResponse extends BaseResponse {

  private static final long serialVersionUID = 800762346877335084L;

  private String warehouseCode;
  private String warehouseName;
  private String contactPerson;
  private long totalArea;
  private long areaAvailable;
  private Date updatedDate;
}
