package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.product.enums.AutoApprovalType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoNeedRevisionDomainEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 3806207661326791709L;
  private String storeId;
  private String productCode;
  private List<String> predictionTypeList = new ArrayList<>();
  private String notes;
  private List<String> vendorErrorFields = new ArrayList<>();
  private boolean contentNeedRevision = false;

  public AutoNeedRevisionDomainEvent(String storeId, String productCode, List<String> predictionTypeList) {
    this.storeId = storeId;
    this.productCode = productCode;
    this.predictionTypeList = predictionTypeList;
  }

  public AutoNeedRevisionDomainEvent(String storeId, String productCode, List<String> predictionTypeList,
      boolean contentNeedRevision) {
    this.storeId = storeId;
    this.productCode = productCode;
    this.predictionTypeList = predictionTypeList;
    this.contentNeedRevision = contentNeedRevision;
  }

  public AutoNeedRevisionDomainEvent(String storeId, String productCode, List<String> predictionTypeList, String notes,
      List<String> vendorErrorFields) {
    this.storeId = storeId;
    this.productCode = productCode;
    this.predictionTypeList = predictionTypeList;
    this.notes = notes;
    this.vendorErrorFields = vendorErrorFields;
  }
}