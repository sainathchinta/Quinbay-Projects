package com.gdn.mta.bulk.entity;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkImageQueueV2 extends GdnBaseDomainEventModel implements Serializable { 
  private static final long serialVersionUID = 4205626195992985829L;
  private String tmpDir;
  private List<ImageTmpMap> imageQueueList;

  public BulkImageQueueV2() {
  }
  
  public BulkImageQueueV2(String tmpDir, List<ImageTmpMap> imageQueueList) {
    super();
    this.tmpDir = tmpDir;
    this.imageQueueList = imageQueueList;
  }
  
  public String getTmpDir() {
    return tmpDir;
  }

  public void setTmpDir(String tmpDir) {
    this.tmpDir = tmpDir;
  }

  public List<ImageTmpMap> getImageQueueList() {
    return imageQueueList;
  }

  public void setImageQueueList(List<ImageTmpMap> imageQueueList) {
    this.imageQueueList = imageQueueList;
  }

}
