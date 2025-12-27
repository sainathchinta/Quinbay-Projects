package com.gdn.mta.product.entity;

import java.io.Serializable;
import java.util.Date;

import jakarta.persistence.Access;
import jakarta.persistence.AccessType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.gdn.GdnBaseEntity;

import lombok.Data;

@Entity
@Data
@Table(name = "PRD_PBP_STOCK_ALERT")
@Access(value = AccessType.FIELD)
public class PbpStockAlert extends GdnBaseEntity implements Serializable {

	private static final long serialVersionUID = 1L;

	@Column(name = "BUSINESS_PARTNER_CODE")
	private String businessPartnerCode;

	@Column(name = "GDN_SKU")
	private String gdnSku;

	@Column(name = "PRODUCT_NAME")
	private String productName;

	@Column(name = "AVAILABLE_STOCK")
	private Integer availableStock;

	@Column(name = "MINIMUM_STOCK")
	private Integer minimumStock;

	@Column(name = "IS_MINIMUM_STOCK")
	private Boolean isMinimumStock;

	@Column(name = "IS_OOS")
	private Boolean isOos;

	@Column(name = "OOS_ALERT_ATTEMPT")
	private Integer oosAlertAttempt;

	@Column(name = "OOS_DATE")
	@Temporal(value = TemporalType.TIMESTAMP)
	private Date oosDate;

	@Column(name = "EVENT_TIMESTAMP")
	@Temporal(value = TemporalType.TIMESTAMP)
	private Date eventTimestamp;

	@Column(name = "Pickup_Point_Code")
	private String pickupPointCode;

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}
