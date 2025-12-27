package com.gdn.x.mta.distributiontask.rest.model.response;

import com.gdn.common.web.base.BaseResponse;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;

/**
 * Created by shripati on 24/10/16.
 */
public class VendorProductStatusResponse extends BaseResponse implements Serializable {

    private static final long serialVersionUID = -5630447703736475879L;
    private Integer inReview;
    private Integer contentApproved;
    private Integer imageApproved;
    private Integer passed;
    private Integer contentAndImageApproved;
    private Integer capacity;

    public Integer getInReview() {
        return inReview;
    }

    public void setInReview(Integer inReview) {
        this.inReview = inReview;
    }

    public Integer getContentApproved() {
        return contentApproved;
    }

    public void setContentApproved(Integer contentApproved) {
        this.contentApproved = contentApproved;
    }

    public Integer getImageApproved() {
        return imageApproved;
    }

    public void setImageApproved(Integer imageApproved) {
        this.imageApproved = imageApproved;
    }

    public Integer getPassed() {
        return passed;
    }

    public void setPassed(Integer passed) {
        this.passed = passed;
    }

    public Integer getContentAndImageApproved() {
        return contentAndImageApproved;
    }

    public void setContentAndImageApproved(Integer contentAndImageApproved) {
        this.contentAndImageApproved = contentAndImageApproved;
    }

    public Integer getCapacity() {
        return capacity;
    }

    public void setCapacity(Integer capacity) {
        this.capacity = capacity;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("inReview", inReview)
            .append("contentApproved", contentApproved).append("imageApproved", imageApproved)
            .append("passed", passed).append("contentAndImageApproved", contentAndImageApproved)
            .append("capacity", capacity).toString();
    }
}
