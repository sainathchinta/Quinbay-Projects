package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class HalalHistoryUpdateEventModel extends GdnBaseDomainEventModel implements Serializable {
    private static final long serialVersionUID = 8718622891457150057L;
    private String storeId;
    private String productSku;
    private String activity;
    private String previousValue;
    private String currentValue;
    private String userName;
}
