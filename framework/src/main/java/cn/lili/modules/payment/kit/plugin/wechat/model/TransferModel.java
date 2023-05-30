package cn.lili.modules.payment.kit.plugin.wechat.model;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * 提现
 * @author Bulbasaur
 */
@Data
@Accessors(chain = true)
public class TransferModel {
    //申请商户号的appid或商户号绑定的appid（企业号corpid即为此appid）
    private String appid;
    //商户系统内部的商家批次单号，要求此参数只能由数字、大小写字母组成，在商户系统内部唯一
    private String out_batch_no;
    //该笔批量转账的名称
    private String batch_name;
    //转账说明，UTF8编码，最多允许32个字符
    private String batch_remark;
    //转账金额单位为“分”。转账总金额必须与批次内所有明细转账金额之和保持一致，否则无法发起转账操作
    private Integer total_amount;
    //一个转账批次单最多发起一千笔转账。转账总笔数必须与批次内所有明细之和保持一致，否则无法发起转账操作
    private Integer total_num;
    //发起批量转账的明细列表，最多一千笔
    private List<TransferDetailInput> transfer_detail_list;
    //必填，指定该笔转账使用的转账场景ID
    private String transfer_scene_id;
}
