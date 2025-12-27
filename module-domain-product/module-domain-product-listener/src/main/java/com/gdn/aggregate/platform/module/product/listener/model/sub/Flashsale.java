package com.gdn.aggregate.platform.module.product.listener.model.sub;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Flashsale {

    private int sequence;

    private String scheduleId;

    private String campaignCode;

    private String type;

    private long start;

    private long end;

    private String logo;

    private String subFlashsaleUrl;

    private boolean timeBased;

    private boolean active;

    private boolean exclusive;

    private List<String> groupIds;

}
