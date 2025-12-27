package com.gdn.x.mta.distributiontask.rest.model.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class PrimaryFilterRequest implements Serializable {

  private static final long serialVersionUID = 8865969917864814936L;

  private String keyword;
  private TimeFilterType timeFilterType;
  private Boolean contentPending;
  private Boolean imagePending;
  private Boolean assignment;
  private String vendorCode;
  private Boolean brandPending;
  private boolean edited;
  private Boolean postLive;
  private boolean revised;
  private Boolean restrictedKeyword;
}
