package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SolrReviewProductCollectionAddEventFields extends GdnBaseDomainEventModel implements Serializable {


  private static final long serialVersionUID = -629050006412099608L;

  private String id;
  private String storeId;
  private String productId;
  private String productCode;
  private String productName;
  private String brand;
  private List<String> categoryCodes;
  private List<String> categoryNames;
  private String businessPartnerCode;
  private String businessPartnerName;
  private Date updatedStepDate;
  private Date updatedDate;
  private Date createdDate;
  private String createdBy;
  private String updatedBy;
  private String assignedTo;
  private boolean activated;
  private boolean viewable;
  private int resubmitCount;
  private Date submittedDate;
  private String state;
  private boolean brandApproved;
}
