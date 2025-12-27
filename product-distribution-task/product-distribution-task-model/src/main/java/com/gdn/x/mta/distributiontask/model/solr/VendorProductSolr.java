package com.gdn.x.mta.distributiontask.model.solr;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VendorProductSolr {
    private String storeId;
    private boolean markForDelete;
    private Date createdDate;
    private String createdBy;
    private Date updatedDate;
    private String productCode;
    private String productName;
    private boolean postLive;
    private List<String> categoryCodes;
    private List<String> categoryNames;
    private String brand;
    private String businessPartnerCode;
    private String businessPartnerName;
    private Date productCreatedDate;
    private int rejectedCount;
    private WorkflowState state;
    private String vendorCode;
    private String productApproverAssignee;
    private Date productAssignedDate;
    private Date productApprovedDate;
    private String brandApprovalStatus;
    private int productPredictionScore;
    private String imageViolations;
    private int qcRetryCount;
    private boolean revised;
    private boolean edited;
    private boolean appealedProduct;
    private boolean restrictedKeywordsPresent;
    private String reviewType;
    private String predictedBrand;
    private String sellerType;
    private String sellerBadge;
    private List<String> productChannel;
    private int distributionMappingStatus;
    private String productCreationType;
}
