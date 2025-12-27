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
public class InternalProcessFailedProductsDownloadRequest extends BulkDownloadRequest implements Serializable {

  private static final long serialVersionUID = -7487249140666743947L;
  private String internalProcessRequestCode;
  private String processType;

  @Override
  public String toString() {
    return "InternalProcessFailedProductsDownloadRequest{" + "internalProcessRequestCode='" + internalProcessRequestCode
        + '\'' + ", processType='" + processType + '\'' + '}';
  }
}
