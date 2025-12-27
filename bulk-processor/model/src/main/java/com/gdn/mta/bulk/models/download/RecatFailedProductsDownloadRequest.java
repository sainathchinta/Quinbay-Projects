package com.gdn.mta.bulk.models.download;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class RecatFailedProductsDownloadRequest extends BulkDownloadRequest implements Serializable {

  private static final long serialVersionUID = -2563250481904970127L;
  private String recatRequestCode;

  @Override
  public String toString() {
    return "RecatFailedProductsDownloadRequest{" + "recatRequestCode='" + recatRequestCode + '\''
        + '}';
  }
}
