package com.gdn.x.mta.distributiontask.model;


import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.Builder;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = VendorDefaultFilter.TABLE_NAME, uniqueConstraints = {
        @UniqueConstraint(columnNames = {VendorDefaultFilter.COLUMN_VENDOR_EMAIL})})
public class VendorDefaultFilter extends GdnBaseEntity {

    private static final long serialVersionUID = -5537122435698733207L;
    public static final String TABLE_NAME = "PDT_VENDOR_DEFAULT_FILTER";
    public static final String COLUMN_VENDOR_EMAIL = "VENDOR_EMAIL";
    public static final String COLUMN_ASSIGNEE_LIST = "ASSIGNEE_LIST";
    public static final String COLUMN_REQUESTED_SKU_COUNT = "REQUESTED_SKU_COUNT";
    @Column(name = COLUMN_VENDOR_EMAIL)
    private String vendorEmail;

    @Column(name = COLUMN_ASSIGNEE_LIST)
    private String assigneeList;

    @Column(name = COLUMN_REQUESTED_SKU_COUNT)
    private int requestedSkuCount;
}
