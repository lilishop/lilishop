package cn.lili.modules.wechat.serviceimpl;

import cn.hutool.http.HttpUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.utils.HttpUtils;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.wechat.entity.dos.WechatMPMessage;
import cn.lili.modules.wechat.entity.enums.WechatMessageItemEnums;
import cn.lili.modules.wechat.mapper.WechatMPMessageMapper;
import cn.lili.modules.wechat.service.WechatMPMessageService;
import cn.lili.modules.wechat.util.WechatAccessTokenUtil;
import cn.lili.modules.wechat.util.WechatMessageUtil;
import cn.lili.mybatis.util.PageUtil;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 微信小程序消息订阅 业务实现
 *
 * @author Chopper
 */
@Service
@Slf4j
public class WechatMPMessageServiceImpl extends ServiceImpl<WechatMPMessageMapper, WechatMPMessage> implements WechatMPMessageService {
    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;

    /**
     * get 获取所有的模版
     */
    private final String allMsgTpl = "https://api.weixin.qq.com/wxaapi/newtmpl/gettemplate?access_token=";
    /**
     * 获取keyid
     */
    private final String keywords = "https://api.weixin.qq.com/wxaapi/newtmpl/getpubtemplatekeywords?access_token=";
    /**
     * post 删除模版
     */
    private final String delMsgTpl = "https://api.weixin.qq.com/wxaapi/newtmpl/deltemplate?access_token=";
    /**
     * post 添加模版
     */
    private final String addTpl = "https://api.weixin.qq.com/wxaapi/newtmpl/addtemplate?access_token=";

    @Override
    public void init() {
        this.baseMapper.deleteAll();
        List<WechatMPMessageData> tmpList = initData();

        // 1. 尝试从微信线上获取已有模板或自动在微信后台创建模板
        Map<String, String> remoteTitleToPriTmplId = new HashMap<>(16);
        String accessToken = null;
        try {
            accessToken = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
            if (StringUtils.isNotEmpty(accessToken)) {
                String context = HttpUtil.get(allMsgTpl + accessToken);
                log.info("获取微信线上全部订阅模版：{}", context);
                JSONObject jsonObject = JSON.parseObject(context);
                if (jsonObject != null && jsonObject.getIntValue("errcode") == 0 && jsonObject.containsKey("data")) {
                    List<String> validTitles = new ArrayList<>();
                    List<String> validCodes = new ArrayList<>();
                    for (WechatMPMessageData d : tmpList) {
                        validTitles.add(d.getTitle());
                        validCodes.add(d.getDefaultCode());
                    }
                    JSONArray dataArr = jsonObject.getJSONArray("data");
                    for (int i = 0; i < dataArr.size(); i++) {
                        JSONObject item = dataArr.getJSONObject(i);
                        if (item != null && StringUtils.isNotEmpty(item.getString("title")) && StringUtils.isNotEmpty(item.getString("priTmplId"))) {
                            String rTitle = item.getString("title").trim();
                            String rPriTmplId = item.getString("priTmplId").trim();
                            if (validTitles.contains(rTitle) || validCodes.contains(rPriTmplId)) {
                                remoteTitleToPriTmplId.put(rTitle, rPriTmplId);
                            } else {
                                Map<String, String> delParams = new HashMap<>(2);
                                delParams.put("priTmplId", rPriTmplId);
                                String delMsg = HttpUtils.doPostWithJson(delMsgTpl + accessToken, delParams);
                                log.info("清理微信线上多余旧订阅模版 [{}]: {}", rTitle, delMsg);
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.warn("连接微信获取已有订阅消息模板异常，将使用预置标准配置初始化", e);
        }

        // 2. 遍历 7 条全量会员小程序订阅消息模板并写入数据库
        for (WechatMPMessageData tplData : tmpList) {
            WechatMPMessage wechatMPMessage = new WechatMPMessage();
            String finalCode = tplData.getDefaultCode();
            List<String> finalKidTexts = tplData.getDefaultKidTexts();

            // 如果微信线上已有同名模板，优先采用线上真实的 priTmplId
            if (remoteTitleToPriTmplId.containsKey(tplData.getTitle())) {
                finalCode = remoteTitleToPriTmplId.get(tplData.getTitle());
            } else if (StringUtils.isNotEmpty(accessToken) && StringUtils.isNotEmpty(tplData.getTid())) {
                // 如果微信线上还没有该模板，自动调微信接口在小程序后台添加该模板
                try {
                    Map<String, Object> params = new HashMap<>(4);
                    params.put("tid", tplData.getTid());
                    String keywordsItems = HttpUtil.get(keywords + accessToken, params);
                    JSONObject kwJson = JSON.parseObject(keywordsItems);
                    if (kwJson != null && kwJson.getIntValue("errcode") == 0 && kwJson.containsKey("data")) {
                        JSONArray jsonArray = kwJson.getJSONArray("data");
                        List<WechatMessageKeyword> keywordArray = JSON.parseArray(JSON.toJSONString(jsonArray), WechatMessageKeyword.class);
                        List<String> kids = new ArrayList<>(tplData.getKeyWord().size());
                        List<String> kidTexts = new ArrayList<>(tplData.getKeyWord().size());
                        keywordLoop:
                        for (WechatMessageItemEnums enums : tplData.getKeyWord()) {
                            for (String tplKey : enums.getText()) {
                                for (WechatMessageKeyword wechatMessageKeyword : keywordArray) {
                                    if (wechatMessageKeyword.getName().equals(tplKey)) {
                                        kids.add(wechatMessageKeyword.getKid());
                                        kidTexts.add(wechatMessageKeyword.getRule() + wechatMessageKeyword.getKid());
                                        continue keywordLoop;
                                    }
                                }
                            }
                        }
                        if (!kids.isEmpty()) {
                            Map<String, Object> addParams = new HashMap<>(4);
                            addParams.put("tid", tplData.getTid());
                            addParams.put("kidList", kids);
                            addParams.put("sceneDesc", tplData.getSceneDesc());
                            String content = HttpUtils.doPostWithJson(addTpl + accessToken, addParams);
                            log.info("自动向微信添加订阅模版参数:{}, 响应:{}", addParams, content);
                            JSONObject tplContent = JSON.parseObject(content);
                            if (tplContent != null && tplContent.containsKey("priTmplId")) {
                                finalCode = tplContent.getString("priTmplId");
                                finalKidTexts = kidTexts;
                            }
                        }
                    }
                } catch (Exception e) {
                    log.warn("自动在微信创建模板 [{}] 失败，使用预置模板ID", tplData.getTitle(), e);
                }
            }

            wechatMPMessage.setName(tplData.getTitle());
            wechatMPMessage.setTemplateId(tplData.getTid());
            wechatMPMessage.setCode(finalCode);
            wechatMPMessage.setKeywords(JSON.toJSONString(tplData.getKeyWord()));
            wechatMPMessage.setKeywordsText(JSON.toJSONString(finalKidTexts));
            wechatMPMessage.setEnable(true);
            wechatMPMessage.setOrderStatus(tplData.getOrderStatus());
            wechatMPMessage.setSceneCode(tplData.getSceneCode());
            this.save(wechatMPMessage);
        }
    }

    @Override
    public IPage<WechatMPMessage> getByPage(WechatMPMessage entity, SearchVO searchVO, PageVO pageVO) {
        return this.page(PageUtil.initPage(pageVO), PageUtil.initWrapper(entity, searchVO));
    }

    public List<WechatMPMessageData> initData() {
        List<WechatMPMessageData> msg = new ArrayList<>();

        // 1. 订单发货通知 (374)
        msg.add(new WechatMPMessageData(
                "订单发货提醒",
                "订单发货通知",
                "374",
                "8UqLGc3kBUDN3md_czOLAcLi_gPBXysS49cVuaSrPMk",
                Arrays.asList(WechatMessageItemEnums.GOODS_INFO, WechatMessageItemEnums.ORDER_SN, WechatMessageItemEnums.LOGISTICS_TIME, WechatMessageItemEnums.REMARK),
                Arrays.asList("thing1", "character_string15", "date4", "thing12"),
                OrderStatusEnum.DELIVERED.name(),
                NoticeMessageNodeEnum.ORDER_DELIVER.name()
        ));

        // 2. 订单完成通知 (3606)
        msg.add(new WechatMPMessageData(
                "订单已完成提醒",
                "订单完成通知",
                "3606",
                "knckkHDSRC7eYuX_4sOJaS_zpHvG3Z13Fzqh78hxBsQ",
                Arrays.asList(WechatMessageItemEnums.GOODS_INFO, WechatMessageItemEnums.ORDER_SN, WechatMessageItemEnums.COMPLETE_TIME, WechatMessageItemEnums.REMARK),
                Arrays.asList("thing3", "character_string5", "time9", "thing4"),
                OrderStatusEnum.COMPLETED.name(),
                NoticeMessageNodeEnum.ORDER_COMPLETE.name()
        ));

        // 3. 拼团成功通知 (766)
        msg.add(new WechatMPMessageData(
                "拼团成功通知",
                "拼团成功通知",
                "766",
                "gdEG644QpBsT_LBnjeCbwlueZx8-5RgCeEVs7y9Ei78",
                Arrays.asList(WechatMessageItemEnums.GOODS_INFO, WechatMessageItemEnums.ORDER_SN, WechatMessageItemEnums.PINTUAN_TIME, WechatMessageItemEnums.REMARK),
                Arrays.asList("thing1", "character_string13", "date7", "thing5"),
                OrderStatusEnum.UNDELIVERED.name(),
                NoticeMessageNodeEnum.PINTUAN_SUCCESS.name()
        ));

        // 4. 售后审核通知 (2927)
        msg.add(new WechatMPMessageData(
                "售后申请审核通知",
                "售后审核通知",
                "2927",
                "j5-2nnrKZOZNpodLJUS2cLjftPUlKJok7m4JZf5-LcM",
                Arrays.asList(WechatMessageItemEnums.GOODS_INFO, WechatMessageItemEnums.AFTER_SALE_SN, WechatMessageItemEnums.AUDIT_RESULT, WechatMessageItemEnums.REMARK),
                Arrays.asList("thing4", "character_string13", "phrase2", "thing3"),
                "AFTER_SALE_AUDIT",
                NoticeMessageNodeEnum.AFTER_SALE_AUDIT.name()
        ));

        // 5. 积分变更通知 (3595)
        msg.add(new WechatMPMessageData(
                "积分变更提醒",
                "积分变更通知",
                "3595",
                "iq4eRz5juxFsFwiRmbu5GPXfBY0w9Kkskf6Kt_m4NnE",
                Arrays.asList(WechatMessageItemEnums.CHANGE_POINT, WechatMessageItemEnums.CURRENT_POINT, WechatMessageItemEnums.POINT_TIME, WechatMessageItemEnums.REMARK),
                Arrays.asList("number2", "thing11", "time12", "thing4"),
                "POINT_CHANGE",
                NoticeMessageNodeEnum.POINT_CHANGE.name()
        ));

        // 6. 余额账户变更通知 (15259)
        msg.add(new WechatMPMessageData(
                "账户余额变动通知",
                "余额账户变更通知",
                "15259",
                "hhuKGE5VzE4TVPJ3ZMmWNs0RRM-HdxmcRdS5ZdOSCZY",
                Arrays.asList(WechatMessageItemEnums.CHANGE_AMOUNT, WechatMessageItemEnums.WALLET_BALANCE, WechatMessageItemEnums.WALLET_TIME, WechatMessageItemEnums.CHANGE_REASON),
                Arrays.asList("character_string1", "amount2", "time3", "thing4"),
                "WALLET_CHANGE",
                NoticeMessageNodeEnum.WALLET_CHANGE.name()
        ));

        // 7. 提现申请审核通知 (20020)
        msg.add(new WechatMPMessageData(
                "提现结果通知",
                "提现申请审核通知",
                "20020",
                "jmXHzErl2ELJ3u3vexC-jJ2eDr3CAzkYi74v39WYkiA",
                Arrays.asList(WechatMessageItemEnums.WITHDRAW_AMOUNT, WechatMessageItemEnums.WITHDRAW_RESULT, WechatMessageItemEnums.APPLY_TIME, WechatMessageItemEnums.WARM_REMIND),
                Arrays.asList("amount1", "phrase2", "date3", "thing4"),
                "WALLET_WITHDRAWAL_AUDIT_ERROR",
                NoticeMessageNodeEnum.WALLET_WITHDRAWAL_AUDIT_ERROR.name()
        ));

        return msg;
    }
}

@Data
@AllArgsConstructor
@NoArgsConstructor
class WechatMPMessageData {

    /**
     * 微信公共模板标题 (如 订单发货提醒)
     */
    String title;

    /**
     * 场景描述 (如 订单发货通知)
     */
    String sceneDesc;

    /**
     * 微信公共模版编号 tid
     */
    String tid;

    /**
     * 默认预置的微信 priTmplId
     */
    String defaultCode;

    /**
     * 自身服务器消息字段枚举
     */
    List<WechatMessageItemEnums> keyWord;

    /**
     * 默认预置的微信变量名列表 (如 thing1, character_string15...)
     */
    List<String> defaultKidTexts;

    /**
     * 状态标识
     */
    String orderStatus;

    /**
     * 与站内信场景一致，对应 {@link cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum#name()}
     */
    String sceneCode;
}

@Data
class WechatMessageKeyword {
    /**
     * id字段
     */
    private String kid;
    /**
     * 名称
     */
    private String name;
    /**
     * 示例值
     */
    private String example;
    /**
     * 示例值
     */
    private String rule;
}
