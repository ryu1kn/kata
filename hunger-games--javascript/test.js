const { expect } = require('chai');
const { nextMorning } = require('./hunger-games');

describe('hungerGame', () => {
  it('Only chicken', () => {
    expect(nextMorning('C')).to.eql('C');
  })

  it('Ex 1', () => {
    expect(nextMorning('CCC[CCC]FCC[CCCCC]CFFFF[CCC]FFFF'))
      .to.eql('...[CCC]F..[CCCCC].FFFF[CCC]FFFF')
  })

  it('Ex 2', () => {
    expect(nextMorning('...[CCC]...[CCCFC].....[CCC]....'))
      .to.eql('...[CCC]...[...F.].....[CCC]....')
  })

  it('Ex 3', () => {
    expect(nextMorning('CCC[CCC]FCC[CCCFC]CFFFF[CCC]FFFF'))
      .to.eql('...[CCC]F..[...F.].FFFF[CCC]FFFF')
  })

  it('Ex 4', () => {
    expect(nextMorning('CCC[CCC][CCCFC][CCC]FFFF'))
      .to.eql('...[CCC][...F.][CCC]FFFF')
  })
})
