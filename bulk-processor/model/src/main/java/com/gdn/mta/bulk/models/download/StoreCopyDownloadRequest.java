package com.gdn.mta.bulk.models.download;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class StoreCopyDownloadRequest extends BulkDownloadRequest implements Serializable {
  private Boolean archived;

  public static class StoreCopyDownloadBuilder extends BulkDownloadRequest.BulkRequestBuilder {
    private Boolean archived;

    public StoreCopyDownloadBuilder() {}

    public StoreCopyDownloadRequest.StoreCopyDownloadBuilder archived(Boolean archived) {
      this.archived = archived;
      return this;
    }

    public StoreCopyDownloadRequest build() {
      return new StoreCopyDownloadRequest(this);
    }
  }

  public StoreCopyDownloadRequest(StoreCopyDownloadRequest.StoreCopyDownloadBuilder builder) {
    super(builder);
    this.archived = builder.archived;
  }
}
