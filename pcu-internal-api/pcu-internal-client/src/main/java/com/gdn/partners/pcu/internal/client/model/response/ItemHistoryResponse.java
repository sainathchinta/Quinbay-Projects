package com.gdn.partners.pcu.internal.client.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemHistoryResponse extends BaseResponse {

  private String itemSku;
  private String action;
  private Date date;
  private String previousMasterSku;
  private String previousAssignee;
  private String currentMasterSku;
  private String assignedTo;
  private AnchorMappingModel anchorMapping;
  private String initiator;
}
