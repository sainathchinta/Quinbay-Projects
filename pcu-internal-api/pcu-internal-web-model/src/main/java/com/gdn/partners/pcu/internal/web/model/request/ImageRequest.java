package com.gdn.partners.pcu.internal.web.model.request;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 13/12/2018 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageRequest {

  private boolean isMainImages;
  private String locationPath;
  private Integer sequence;
  private String id;
  private String storeId;
  private Long version;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private boolean markForDelete;
  private String hashCode;
  private boolean active;
  private Boolean originalImage;
  private boolean edited;
  private boolean commonImage;
  private String type;
}
