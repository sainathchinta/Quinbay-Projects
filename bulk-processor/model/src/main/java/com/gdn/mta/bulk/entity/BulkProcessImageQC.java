package com.gdn.mta.bulk.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Table(name = BulkProcessImageQC.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {BulkProcessImageQC.COLUMN_PARENT_PRODUCT,
        BulkProcessImageQC.COLUMN_BULK_PROCESS_CODE})})
public class BulkProcessImageQC extends GdnBaseEntity {
    public static final String TABLE_NAME = "BLP_BULK_PROCESS_IMAGE_QC";
    public static final String COLUMN_BULK_PROCESS_CODE = "BULK_PROCESS_CODE";
    public static final String COLUMN_PARENT_PRODUCT = "PARENT_PRODUCT";
    public static final String COLUMN_EXTERNAL_CATEGORY = "EXTERNAL_CATEGORY";
    public static final String COLUMN_DS_RESPONSE = "DS_RESPONSE";
    public static final String COLUMN_COMPLETED = "COMPLETED";
    public static final String COLUMN_RETRY_COUNT = "RETRY_COUNT";

    @Column(name = BulkProcessImageQC.COLUMN_BULK_PROCESS_CODE, nullable = false)
    private String bulkProcessCode;

    @Column(name = BulkProcessImageQC.COLUMN_PARENT_PRODUCT, nullable = false)
    private String parentProduct;

    @Column(name = BulkProcessImageQC.COLUMN_EXTERNAL_CATEGORY)
    private String externalCategory;

    @Column(name = BulkProcessImageQC.COLUMN_DS_RESPONSE)
    private String dsResponse;

    @Column(name = BulkProcessImageQC.COLUMN_COMPLETED)
    private boolean completed;

    @Column(name = BulkProcessImageQC.COLUMN_RETRY_COUNT)
    private int retryCount;
}
