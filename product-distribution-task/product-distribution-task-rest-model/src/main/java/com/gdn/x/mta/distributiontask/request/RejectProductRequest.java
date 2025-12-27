package com.gdn.x.mta.distributiontask.request;

/**
 * Created by Poornima on 9/24/16.
 */
public class RejectProductRequest {

    private String productId;
    private String rejectedType;
    private String rejectedReason;
    private boolean isAssignedToVendor;

    public RejectProductRequest() {
        // No implementation
    }

    public RejectProductRequest(String productId, String rejectedType, String rejectedReason,
        boolean isAssignedToVendor) {
        this.productId = productId;
        this.rejectedType = rejectedType;
        this.rejectedReason = rejectedReason;
        this.isAssignedToVendor = isAssignedToVendor;
    }

    public String getProductId() {
        return productId;
    }

    public void setProductId(String productId) {
        this.productId = productId;
    }

    public String getRejectedType() {
        return rejectedType;
    }

    public void setRejectedType(String rejectedType) {
        this.rejectedType = rejectedType;
    }

    public String getRejectedReason() {
        return rejectedReason;
    }

    public void setRejectedReason(String rejectedReason) {
        this.rejectedReason = rejectedReason;
    }

    public boolean isAssignedToVendor() {
        return isAssignedToVendor;
    }

    public void setAssignedToVendor(boolean assignedToVendor) {
        isAssignedToVendor = assignedToVendor;
    }

    @Override public String toString() {
        final StringBuilder stringBuilder = new StringBuilder("RejectProductRequest{");
        stringBuilder.append("productCode='").append(productId).append('\'');
        stringBuilder.append("rejectedType='").append(rejectedType).append('\'');
        stringBuilder.append("rejectedReason='").append(rejectedReason).append('\'');
        stringBuilder.append("isAssignedToVendor='").append(isAssignedToVendor);
        return stringBuilder.toString();
    }
}
