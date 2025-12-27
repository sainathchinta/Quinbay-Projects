package com.gdn.mta.bulk.models;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class IprActionRequest implements Serializable {
  private static final long serialVersionUID = 4157135592970777405L;

  private String productSku;
  private String action;
  private String sellerNotes;
  private String reviewerNotes;
  private String reasons;
  private String violationType;
  private String updatedBy;
  private String source;
  private boolean bulkAction;
  private String storeId;
  private String assignee;
  private BrandReport brandReport;
}