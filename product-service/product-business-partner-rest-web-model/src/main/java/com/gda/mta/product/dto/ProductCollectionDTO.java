package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.Date;

@EqualsAndHashCode(callSuper = true)
@Builder
@Data
@ToString
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCollectionDTO extends BaseResponse {
    private String productId;
    private String productCode;
    private String productName;
    private String brand;
    private String brandCode;
    private BrandApprovalStatus brandApprovalStatus;
    private AutoApprovalType autoApprovalType;
    private String categoryCode;
    private String categoryName;
    private String businessPartnerCode;
    private String businessPartnerName;
    private boolean activated;
    private boolean viewable;
    private Date updatedStepDate;
    private String state;
    private Date submittedDate;
    private Integer approvalStatus;
    private Date approvalStatusTimestamp;
    private Integer approveRetryCount;
    private int stuckProductRetryCount;
    private int resubmitCount;
    private String assignedTo;
    private String assignedBy;
    private String reviewerNotes;
    private boolean bulkCreated;
    private boolean imageResized;
    private boolean reviewPending;
    private boolean postLive;
    private boolean restrictedKeywordsPresent;
    private int imageQcState;
    private String productCreationType;
    private boolean skipReview;
    private boolean edited;
    private String reviewType;
    private String needCorrectionNotes;
    private String restrictedKeywordsDetected;
    private boolean autoNeedRevision;
    private int autoNeedRevisionCount;
    private int imageResizeRetry;
    private boolean needRevision;
    private int prioritySeller;
    private boolean markForDelete;
}
