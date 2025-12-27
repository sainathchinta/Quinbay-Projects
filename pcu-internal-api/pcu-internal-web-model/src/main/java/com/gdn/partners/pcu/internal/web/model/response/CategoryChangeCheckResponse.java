package com.gdn.partners.pcu.internal.web.model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude
public class CategoryChangeCheckResponse implements Serializable {

  private static final long serialVersionUID = -3109558655769848523L;

  private boolean categoryChangeSupported;
  private String reason;
}
