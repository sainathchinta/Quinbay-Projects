package com.gdn.partners.pcu.internal.web.model.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@JsonInclude(JsonInclude.Include.ALWAYS)
public class SuspensionProductBulkActionsWebRequest implements Serializable {

  private static final long serialVersionUID = 7329800794651659306L;

  private String action;
  private String reason;
  private String notes;
  private List<ProductSuspensionWebRequest> products = new ArrayList<>();
}
