package com.gdn.mta.product.entity;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemLevel3Order implements Serializable {

	private static final long serialVersionUID = 4030360904569422482L;
	private String skuCode;
	private Double length;
	private Double width;
	private Double height;
	private Boolean off2OnActiveFlag;
	private List<ProductLevel3Price> prices = new ArrayList<>();
	private String merchantSku;

	public ProductItemLevel3Order() {
		// do nothing
	}

	public ProductItemLevel3Order(String skuCode, Double length, Double width,
			Double height) {
		super();
		this.skuCode = skuCode;
		this.length = length;
		this.width = width;
		this.height = height;
	}

	public String getMerchantSku() { return merchantSku; }

	public void setMerchantSku(String merchantSku) { this.merchantSku = merchantSku; }

	public String getSkuCode() {
		return skuCode;
	}

	public void setSkuCode(String skuCode) {
		this.skuCode = skuCode;
	}

	public Double getLength() {
		return length;
	}

	public void setLength(Double length) {
		this.length = length;
	}

	public Double getWidth() {
		return width;
	}

	public void setWidth(Double width) {
		this.width = width;
	}

	public Double getHeight() {
		return height;
	}
	
	public void setHeight(Double height) {
		this.height = height;
	}

	public Boolean getOff2OnActiveFlag() {
		return off2OnActiveFlag;
	}

	public void setOff2OnActiveFlag(Boolean off2OnActiveFlag) {
		this.off2OnActiveFlag = off2OnActiveFlag;
	}

	public List<ProductLevel3Price> getPrices() {
		return prices;
	}

	public void setPrices(List<ProductLevel3Price> prices) {
		this.prices = prices;
	}

	@Override
	public String toString() {
		return String
				.format("ProductItemLevel3Order [skuCode=%s, length=%s, width=%s, height=%s, getSkuCode()=%s, getLength()=%s, getWidth()=%s, getHeight()=%s, off2OnActiveFlag=%s, prices=%s, merchantSku=%s]",
						skuCode, length, width, height, getSkuCode(),
						getLength(), getWidth(), getHeight(), getOff2OnActiveFlag(), getPrices(), getMerchantSku());
	}

}
