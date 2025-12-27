package com.gdn.x.mta.distributiontask.model.dto;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

/**
 * Created by shripati on 24/10/16.
 */
public class VendorProductStatusDTO {
    private WorkflowState state;
    private Integer count;

    public VendorProductStatusDTO(WorkflowState state, Long count) {
        this.state = state;
        this.count = count != null ? count.intValue() : 0;
    }

    public WorkflowState getState() {
        return state;
    }

    public void setState(WorkflowState state) {
        this.state = state;
    }

    public Integer getCount() {
        return count;
    }

    public void setCount(Integer count) {
        this.count = count;
    }
}
