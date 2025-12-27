package com.gdn.mta.bulk.entity;

import java.io.Serializable;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

/**
 * Created by hardikbohra on 14/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkImageQueue extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 7723325279876681520L;

  private List<ImageQueue> imageQueueList;

  public BulkImageQueue() {
  }
  
  public BulkImageQueue(List<ImageQueue> imageQueueList) {
    super();
    this.imageQueueList = imageQueueList;
  }

  public List<ImageQueue> getImageQueueList() {
    return imageQueueList;
  }

  public void setImageQueueList(List<ImageQueue> imageQueueList) {
    this.imageQueueList = imageQueueList;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("imageQueueList", imageQueueList).toString();
  }
}
