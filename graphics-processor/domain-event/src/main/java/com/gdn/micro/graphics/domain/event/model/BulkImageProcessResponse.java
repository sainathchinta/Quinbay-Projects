package com.gdn.micro.graphics.domain.event.model;

import java.io.Serializable;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

/**
 * Created by Vishal on 10/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkImageProcessResponse  extends GdnBaseDomainEventModel implements Serializable{

  private static final long serialVersionUID = -7639145600963148736L;
  private String groupCode;
  private String storeId;
  private String username;
  private List<ImageResponse> imageResponses;

  public BulkImageProcessResponse() {
  }

  public String getGroupCode() {
    return groupCode;
  }

  public void setGroupCode(String groupCode) {
    this.groupCode = groupCode;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public List<ImageResponse> getImageResponses() {
    return imageResponses;
  }

  public void setImageResponses(List<ImageResponse> imageResponses) {
    this.imageResponses = imageResponses;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("groupCode", groupCode).append("storeId", storeId)
        .append("username", username).append("imageResponses", imageResponses).toString();
  }
}
