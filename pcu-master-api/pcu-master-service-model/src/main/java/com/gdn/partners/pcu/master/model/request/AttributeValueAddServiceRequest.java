package com.gdn.partners.pcu.master.model.request;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Srividhya
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeValueAddServiceRequest {

  @NotNull(message = "Value should not be null")
  private String value;
  private String createdBy;
  private Date createdDate;
  private int sequence;
}