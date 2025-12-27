package com.gdn.mta.product.util;

import com.gdn.common.web.base.BaseResponse;

public class SingleValueResponse  extends BaseResponse{

    private static final long serialVersionUID = 1121418119277785992L;
    private String value;

    @Deprecated
    public SingleValueResponse() {
    }

    public SingleValueResponse(String value)
    {
        super(null,null,null,null,null,null);
        this.value = value;
    }

    public String getValue() {
        return this.value;
    }

    public void setValue(String value) {
        this.value = value;
    }
}
