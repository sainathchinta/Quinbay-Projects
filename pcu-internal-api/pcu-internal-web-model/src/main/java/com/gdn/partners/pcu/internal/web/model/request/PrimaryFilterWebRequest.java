package com.gdn.partners.pcu.internal.web.model.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@JsonInclude(JsonInclude.Include.ALWAYS)
public class PrimaryFilterWebRequest implements Serializable {

  private static final long serialVersionUID = 7116614406478905890L;

  private String keyword;
  private String timeFilterWebType;
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
