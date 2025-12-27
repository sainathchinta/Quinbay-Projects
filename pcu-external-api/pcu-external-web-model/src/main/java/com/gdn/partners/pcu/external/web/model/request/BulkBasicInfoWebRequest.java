package com.gdn.partners.pcu.external.web.model.request;

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
@Builder
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkBasicInfoWebRequest implements Serializable {
  private static final long serialVersionUID = -5049391291167153251L;
  private String fileName;
  private String filePath;
  private String bulkProcessCode;
}
