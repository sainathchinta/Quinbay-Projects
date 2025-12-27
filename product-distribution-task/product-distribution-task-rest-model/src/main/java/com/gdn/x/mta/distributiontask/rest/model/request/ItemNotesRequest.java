package com.gdn.x.mta.distributiontask.rest.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
@Builder
public class ItemNotesRequest extends BaseRequest {
  private String itemSku;
  private String skuCode;
  private Integer itemNumber;
  private String itemName;
  private List<String> vendorNotes;
  private List<String> vendorErrorFields;
}
