package com.gdn.partners.pcu.internal.client.model.request;

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
  private static final long serialVersionUID = 8322680266470617489L;

  private String productSku;
  private String action;
  private String sellerNotes;
  private String reviewerNotes;
  private String reasons;
  private String violationType;
  private String updatedBy;
  private String violatedFields;
}